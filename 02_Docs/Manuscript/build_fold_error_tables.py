from __future__ import annotations

import csv
import math
import re
import zipfile
from pathlib import Path
from xml.etree import ElementTree as ET
from xml.sax.saxutils import escape


ROOT = Path(__file__).resolve().parents[2]
APRIORI = ROOT / "03_APriori/SimsOutputs/Figures/PK_Metrics_ObsPred.xlsx"
TOPDOWN_MONKEY = ROOT / "04_TopDown/SimsOutputs/TopDownTMDD_metrics_Monkey.csv"
TOPDOWN_HUMAN = ROOT / "04_TopDown/SimsOutputs/TopDownTMDD_to_Human_metrics.csv"
OUT_DIR = ROOT / "02_Docs/Manuscript"
DOCX_OUT = OUT_DIR / "Fold_Error_Comparison_Tables.docx"
XLSX_OUT = OUT_DIR / "Fold_Error_Comparison_Tables.xlsx"

GREEN = "E2F0D9"
RED = "F4CCCC"
HEADER = "D9E2F3"
SUBHEADER = "EAF0F8"
WHITE = "FFFFFF"
BLACK = "000000"
GRID = "7F7F7F"


def col_number(cell_ref: str) -> int:
    letters = re.match(r"[A-Z]+", cell_ref).group(0)
    n = 0
    for char in letters:
        n = n * 26 + ord(char) - 64
    return n


def read_first_xlsx_sheet(path: Path) -> list[dict[str, str]]:
    ns = {
        "m": "http://schemas.openxmlformats.org/spreadsheetml/2006/main",
        "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
    }
    with zipfile.ZipFile(path) as zf:
        workbook = ET.fromstring(zf.read("xl/workbook.xml"))
        relationships = ET.fromstring(zf.read("xl/_rels/workbook.xml.rels"))
        rels = {node.attrib["Id"]: node.attrib["Target"] for node in relationships}

        shared_strings = []
        if "xl/sharedStrings.xml" in zf.namelist():
            shared = ET.fromstring(zf.read("xl/sharedStrings.xml"))
            for item in shared.findall("m:si", ns):
                shared_strings.append(
                    "".join(t.text or "" for t in item.iter(f"{{{ns['m']}}}t"))
                )

        sheet = next(iter(workbook.find("m:sheets", ns)))
        target = rels[sheet.attrib[f"{{{ns['r']}}}id"]]
        target = target.lstrip("/")
        if not target.startswith("xl/"):
            target = "xl/" + target
        worksheet = ET.fromstring(zf.read(target))

        matrix: list[list[str]] = []
        for row in worksheet.findall(".//m:sheetData/m:row", ns):
            values: dict[int, str] = {}
            for cell in row.findall("m:c", ns):
                index = col_number(cell.attrib["r"]) - 1
                cell_type = cell.attrib.get("t")
                value_node = cell.find("m:v", ns)
                value = "" if value_node is None else value_node.text or ""
                if cell_type == "s" and value:
                    value = shared_strings[int(value)]
                elif cell_type == "inlineStr":
                    value = "".join(
                        t.text or "" for t in cell.iter(f"{{{ns['m']}}}t")
                    )
                values[index] = value
            if values:
                matrix.append([values.get(i, "") for i in range(max(values) + 1)])

    headers = matrix[0]
    return [
        {header: row[i] if i < len(row) else "" for i, header in enumerate(headers)}
        for row in matrix[1:]
    ]


def dose_key(value: str | float) -> float:
    return round(float(value), 8)


def load_data() -> dict[str, list[dict[str, object]]]:
    apriori_rows = read_first_xlsx_sheet(APRIORI)
    apriori = {}
    for row in apriori_rows:
        key = (row["Species"], row["Molecule"], dose_key(row["Dose (mg/kg)"]))
        apriori[key] = {
            "ap_cmax": float(row["Fold Error Cmax"]),
            "ap_auc": float(row["Fold Error AUC"]),
        }

    topdown_rows = []
    with TOPDOWN_MONKEY.open(newline="", encoding="utf-8-sig") as handle:
        for row in csv.DictReader(handle):
            if row["Run"] == "Run3_CL_kint_fixed_Rtot":
                row["Species"] = "Monkey"
                topdown_rows.append(row)
    with TOPDOWN_HUMAN.open(newline="", encoding="utf-8-sig") as handle:
        for row in csv.DictReader(handle):
            row["Species"] = "Human"
            topdown_rows.append(row)

    result = {"Monkey": [], "Human": []}
    for row in topdown_rows:
        species = row["Species"]
        key = (species, row["Molecule"], dose_key(row["Dose"]))
        if key not in apriori:
            continue
        result[species].append(
            {
                "molecule": row["Molecule"],
                "dose": float(row["Dose"]),
                **apriori[key],
                "td_cmax": float(row["FE_Cmax"]),
                "td_auc": float(row["FE_AUC"]),
            }
        )

    for species in result:
        result[species].sort(key=lambda x: (str(x["molecule"]).lower(), float(x["dose"])))
    return result


def within_twofold(value: float) -> bool:
    return 0.5 <= value <= 2.0


def format_dose(value: float) -> str:
    return str(int(value)) if value.is_integer() else f"{value:g}"


def w_text(text: str, *, bold: bool = False, size: int = 18) -> str:
    rpr = []
    if bold:
        rpr.append("<w:b/>")
    rpr.append(f'<w:sz w:val="{size}"/><w:szCs w:val="{size}"/>')
    return (
        "<w:p><w:pPr><w:spacing w:after=\"0\"/></w:pPr><w:r><w:rPr>"
        + "".join(rpr)
        + f'</w:rPr><w:t xml:space="preserve">{escape(text)}</w:t></w:r></w:p>'
    )


def w_cell(
    text: str,
    width: int,
    *,
    fill: str = WHITE,
    bold: bool = False,
    align: str = "center",
    colspan: int = 1,
) -> str:
    gridspan = f'<w:gridSpan w:val="{colspan}"/>' if colspan > 1 else ""
    return (
        "<w:tc><w:tcPr>"
        f'<w:tcW w:w="{width}" w:type="dxa"/>{gridspan}'
        f'<w:shd w:val="clear" w:color="auto" w:fill="{fill}"/>'
        '<w:tcMar><w:top w:w="60" w:type="dxa"/><w:left w:w="80" w:type="dxa"/>'
        '<w:bottom w:w="60" w:type="dxa"/><w:right w:w="80" w:type="dxa"/></w:tcMar>'
        "</w:tcPr>"
        f'<w:p><w:pPr><w:jc w:val="{align}"/><w:spacing w:after="0"/></w:pPr>'
        f'<w:r><w:rPr>{"<w:b/>" if bold else ""}<w:sz w:val="17"/><w:szCs w:val="17"/></w:rPr>'
        f"<w:t>{escape(text)}</w:t></w:r></w:p></w:tc>"
    )


def docx_table(species: str, rows: list[dict[str, object]]) -> str:
    widths = [2200, 900, 1350, 1350, 1350, 1350]
    table = [
        "<w:tbl><w:tblPr><w:tblW w:w=\"8500\" w:type=\"dxa\"/>"
        '<w:tblLayout w:type="fixed"/>'
        '<w:tblBorders><w:top w:val="single" w:sz="8" w:color="7F7F7F"/>'
        '<w:left w:val="single" w:sz="4" w:color="B7B7B7"/>'
        '<w:bottom w:val="single" w:sz="8" w:color="7F7F7F"/>'
        '<w:right w:val="single" w:sz="4" w:color="B7B7B7"/>'
        '<w:insideH w:val="single" w:sz="4" w:color="B7B7B7"/>'
        '<w:insideV w:val="single" w:sz="4" w:color="B7B7B7"/></w:tblBorders>'
        "</w:tblPr>"
    ]
    table.append(
        '<w:tr><w:trPr><w:tblHeader/></w:trPr>'
        + w_cell("Molecule", widths[0], fill=HEADER, bold=True)
        + w_cell("Dose\n(mg/kg)", widths[1], fill=HEADER, bold=True)
        + w_cell("Cmax fold-error", widths[2] + widths[3], fill=HEADER, bold=True, colspan=2)
        + w_cell("AUC fold-error", widths[4] + widths[5], fill=HEADER, bold=True, colspan=2)
        + "</w:tr>"
    )
    table.append(
        '<w:tr><w:trPr><w:tblHeader/></w:trPr>'
        + w_cell("", widths[0], fill=SUBHEADER)
        + w_cell("", widths[1], fill=SUBHEADER)
        + w_cell("A priori", widths[2], fill=SUBHEADER, bold=True)
        + w_cell("Top-down", widths[3], fill=SUBHEADER, bold=True)
        + w_cell("A priori", widths[4], fill=SUBHEADER, bold=True)
        + w_cell("Top-down", widths[5], fill=SUBHEADER, bold=True)
        + "</w:tr>"
    )
    for row in rows:
        cells = [
            w_cell(str(row["molecule"]), widths[0], align="left"),
            w_cell(format_dose(float(row["dose"])), widths[1]),
        ]
        for key, width in zip(
            ["ap_cmax", "td_cmax", "ap_auc", "td_auc"], widths[2:]
        ):
            value = float(row[key])
            cells.append(
                w_cell(
                    f"{value:.2f}",
                    width,
                    fill=GREEN if within_twofold(value) else RED,
                )
            )
        table.append("<w:tr>" + "".join(cells) + "</w:tr>")
    table.append("</w:tbl>")
    return "".join(table)


def build_docx(data: dict[str, list[dict[str, object]]]) -> None:
    content = []
    for index, species in enumerate(("Monkey", "Human"), start=1):
        content.append(w_text(f"Table {index}. {species} Cmax and AUC fold-error by modeling approach", bold=True, size=20))
        content.append(
            w_text(
                "Fold-error is reported as predicted/observed. Green indicates within twofold "
                "(0.50–2.00); red indicates outside twofold.",
                size=17,
            )
        )
        content.append(docx_table(species, data[species]))
        if index == 1:
            content.append('<w:p><w:r><w:br w:type="page"/></w:r></w:p>')

    document = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">'
        "<w:body>"
        + "".join(content)
        + '<w:sectPr><w:pgSz w:w="12240" w:h="15840"/>'
        '<w:pgMar w:top="720" w:right="720" w:bottom="720" w:left="720" '
        'w:header="360" w:footer="360" w:gutter="0"/></w:sectPr></w:body></w:document>'
    )
    styles = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<w:styles xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">'
        '<w:style w:type="paragraph" w:default="1" w:styleId="Normal">'
        '<w:name w:val="Normal"/><w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial"/>'
        '<w:sz w:val="18"/><w:szCs w:val="18"/></w:rPr></w:style></w:styles>'
    )
    content_types = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
        '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>'
        '<Default Extension="xml" ContentType="application/xml"/>'
        '<Override PartName="/word/document.xml" '
        'ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.document.main+xml"/>'
        '<Override PartName="/word/styles.xml" '
        'ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.styles+xml"/>'
        "</Types>"
    )
    root_rels = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
        '<Relationship Id="rId1" '
        'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" '
        'Target="word/document.xml"/></Relationships>'
    )
    doc_rels = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
        '<Relationship Id="rId1" '
        'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" '
        'Target="styles.xml"/></Relationships>'
    )
    with zipfile.ZipFile(DOCX_OUT, "w", zipfile.ZIP_DEFLATED) as zf:
        zf.writestr("[Content_Types].xml", content_types)
        zf.writestr("_rels/.rels", root_rels)
        zf.writestr("word/document.xml", document)
        zf.writestr("word/styles.xml", styles)
        zf.writestr("word/_rels/document.xml.rels", doc_rels)


def xlsx_cell(ref: str, value: str | float, style: int) -> str:
    if isinstance(value, (int, float)):
        return f'<c r="{ref}" s="{style}"><v>{value}</v></c>'
    return (
        f'<c r="{ref}" s="{style}" t="inlineStr"><is><t>{escape(value)}</t></is></c>'
    )


def build_sheet(rows: list[dict[str, object]]) -> str:
    columns = ["A", "B", "C", "D", "E", "F"]
    output = [
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
        '<sheetViews><sheetView workbookViewId="0"><pane ySplit="2" topLeftCell="A3" '
        'activePane="bottomLeft" state="frozen"/></sheetView></sheetViews>',
        '<cols><col min="1" max="1" width="22" customWidth="1"/>'
        '<col min="2" max="2" width="13" customWidth="1"/>'
        '<col min="3" max="6" width="17" customWidth="1"/></cols>',
        "<sheetData>",
    ]
    header1 = ["Molecule", "Dose (mg/kg)", "Cmax fold-error", "", "AUC fold-error", ""]
    header2 = ["", "", "A priori", "Top-down", "A priori", "Top-down"]
    output.append("<row r=\"1\" ht=\"26\" customHeight=\"1\">")
    for col, value in zip(columns, header1):
        output.append(xlsx_cell(f"{col}1", value, 1))
    output.append("</row><row r=\"2\" ht=\"22\" customHeight=\"1\">")
    for col, value in zip(columns, header2):
        output.append(xlsx_cell(f"{col}2", value, 2))
    output.append("</row>")
    for excel_row, row in enumerate(rows, start=3):
        values = [
            str(row["molecule"]),
            float(row["dose"]),
            float(row["ap_cmax"]),
            float(row["td_cmax"]),
            float(row["ap_auc"]),
            float(row["td_auc"]),
        ]
        output.append(f'<row r="{excel_row}">')
        for col, value in zip(columns, values):
            style = 3 if col in ("A", "B") else (4 if within_twofold(float(value)) else 5)
            output.append(xlsx_cell(f"{col}{excel_row}", value, style))
        output.append("</row>")
    last_row = len(rows) + 2
    output.extend(
        [
            "</sheetData>",
            f'<autoFilter ref="A2:F{last_row}"/>',
            '<mergeCells count="2"><mergeCell ref="C1:D1"/><mergeCell ref="E1:F1"/></mergeCells>',
            '<pageMargins left="0.25" right="0.25" top="0.5" bottom="0.5" header="0.2" footer="0.2"/>',
            '<pageSetup orientation="portrait" fitToWidth="1" fitToHeight="0"/>',
            "</worksheet>",
        ]
    )
    return "".join(output)


def build_xlsx(data: dict[str, list[dict[str, object]]]) -> None:
    content_types = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">'
        '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>'
        '<Default Extension="xml" ContentType="application/xml"/>'
        '<Override PartName="/xl/workbook.xml" '
        'ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>'
        '<Override PartName="/xl/styles.xml" '
        'ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>'
        '<Override PartName="/xl/worksheets/sheet1.xml" '
        'ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>'
        '<Override PartName="/xl/worksheets/sheet2.xml" '
        'ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>'
        "</Types>"
    )
    root_rels = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
        '<Relationship Id="rId1" '
        'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" '
        'Target="xl/workbook.xml"/></Relationships>'
    )
    workbook = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" '
        'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">'
        '<sheets><sheet name="Monkey" sheetId="1" r:id="rId1"/>'
        '<sheet name="Human" sheetId="2" r:id="rId2"/></sheets></workbook>'
    )
    workbook_rels = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
        '<Relationship Id="rId1" '
        'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" '
        'Target="worksheets/sheet1.xml"/>'
        '<Relationship Id="rId2" '
        'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" '
        'Target="worksheets/sheet2.xml"/>'
        '<Relationship Id="rId3" '
        'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" '
        'Target="styles.xml"/></Relationships>'
    )
    styles = f"""<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">
  <numFmts count="1"><numFmt numFmtId="164" formatCode="0.00"/></numFmts>
  <fonts count="2">
    <font><sz val="10"/><name val="Arial"/></font>
    <font><b/><sz val="10"/><name val="Arial"/></font>
  </fonts>
  <fills count="5">
    <fill><patternFill patternType="none"/></fill>
    <fill><patternFill patternType="gray125"/></fill>
    <fill><patternFill patternType="solid"><fgColor rgb="FF{HEADER}"/><bgColor indexed="64"/></patternFill></fill>
    <fill><patternFill patternType="solid"><fgColor rgb="FF{GREEN}"/><bgColor indexed="64"/></patternFill></fill>
    <fill><patternFill patternType="solid"><fgColor rgb="FF{RED}"/><bgColor indexed="64"/></patternFill></fill>
  </fills>
  <borders count="2">
    <border><left/><right/><top/><bottom/><diagonal/></border>
    <border><left style="thin"><color rgb="FFB7B7B7"/></left><right style="thin"><color rgb="FFB7B7B7"/></right>
      <top style="thin"><color rgb="FFB7B7B7"/></top><bottom style="thin"><color rgb="FFB7B7B7"/></bottom><diagonal/></border>
  </borders>
  <cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>
  <cellXfs count="6">
    <xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>
    <xf numFmtId="0" fontId="1" fillId="2" borderId="1" xfId="0" applyAlignment="1"><alignment horizontal="center" vertical="center" wrapText="1"/></xf>
    <xf numFmtId="0" fontId="1" fillId="2" borderId="1" xfId="0" applyAlignment="1"><alignment horizontal="center" vertical="center"/></xf>
    <xf numFmtId="0" fontId="0" fillId="0" borderId="1" xfId="0" applyAlignment="1"><alignment horizontal="center"/></xf>
    <xf numFmtId="164" fontId="0" fillId="3" borderId="1" xfId="0" applyNumberFormat="1" applyAlignment="1"><alignment horizontal="center"/></xf>
    <xf numFmtId="164" fontId="0" fillId="4" borderId="1" xfId="0" applyNumberFormat="1" applyAlignment="1"><alignment horizontal="center"/></xf>
  </cellXfs>
  <cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>
</styleSheet>"""
    with zipfile.ZipFile(XLSX_OUT, "w", zipfile.ZIP_DEFLATED) as zf:
        zf.writestr("[Content_Types].xml", content_types)
        zf.writestr("_rels/.rels", root_rels)
        zf.writestr("xl/workbook.xml", workbook)
        zf.writestr("xl/_rels/workbook.xml.rels", workbook_rels)
        zf.writestr("xl/styles.xml", styles)
        zf.writestr("xl/worksheets/sheet1.xml", build_sheet(data["Monkey"]))
        zf.writestr("xl/worksheets/sheet2.xml", build_sheet(data["Human"]))


if __name__ == "__main__":
    OUT_DIR.mkdir(parents=True, exist_ok=True)
    matched = load_data()
    build_docx(matched)
    build_xlsx(matched)
    print(f"Monkey rows: {len(matched['Monkey'])}")
    print(f"Human rows: {len(matched['Human'])}")
    print(DOCX_OUT)
    print(XLSX_OUT)

from __future__ import annotations

import csv
import math
import runpy
import zipfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
BASE = runpy.run_path(str(Path(__file__).with_name("build_fold_error_tables.py")))
DOCX_OUT = Path(__file__).with_name("Pembrolizumab_Human_Fold_Error_Table.docx")
XLSX_OUT = Path(__file__).with_name("Pembrolizumab_Human_Fold_Error_Table.xlsx")
SIM_DIR = ROOT / "03_APriori/SimsOutputs/SimulationResults/2026-06-24 09-26"


def auc(points: list[tuple[float, float]]) -> float:
    total = 0.0
    for (t1, c1), (t2, c2) in zip(points, points[1:]):
        dt = t2 - t1
        if dt <= 0 or c1 <= 0 or c2 <= 0:
            continue
        total += (
            dt * (c1 - c2) / math.log(c1 / c2)
            if c1 != c2
            else dt * (c1 + c2) / 2
        )
    return total


def calculate_rows() -> list[dict[str, object]]:
    observed: dict[float, list[tuple[float, float]]] = {}
    with (ROOT / "01_Data/Human/mAb_data_human.csv").open(
        newline="", encoding="utf-8-sig"
    ) as handle:
        for row in csv.DictReader(handle):
            if row["Molecule"] != "Pembrolizumab":
                continue
            time = float(row["Time"])
            concentration = float(row["Measurement"])
            if time <= 672 and concentration > 0:
                observed.setdefault(float(row["Dose"]), []).append(
                    (time, concentration)
                )

    topdown = {}
    with (ROOT / "04_TopDown/SimsOutputs/TopDownTMDD_to_Human_metrics.csv").open(
        newline="", encoding="utf-8-sig"
    ) as handle:
        for row in csv.DictReader(handle):
            if row["Molecule"] == "Pembrolizumab":
                topdown[float(row["Dose"])] = {
                    "td_cmax": float(row["FE_Cmax"]),
                    "td_auc": float(row["FE_AUC"]),
                }

    rows = []
    for dose in (2.0, 10.0):
        simulation = SIM_DIR / f"Pembrolizumab_Human_{dose:g}_mpk.csv"
        individuals: dict[int, list[tuple[float, float]]] = {}
        with simulation.open(newline="", encoding="utf-8-sig") as handle:
            reader = csv.DictReader(handle)
            concentration_column = next(
                name
                for name in reader.fieldnames or []
                if "PeripheralVenousBlood" in name and "|mAb|" in name
            )
            for row in reader:
                time = float(row["Time [min]"]) / 60
                if time <= 672:
                    concentration = float(row[concentration_column]) * 146000 / 1000
                    individuals.setdefault(int(row["IndividualId"]), []).append(
                        (time, concentration)
                    )

        pred_auc = sum(auc(sorted(points)) for points in individuals.values()) / len(
            individuals
        )
        pred_cmax = sum(
            max(concentration for _, concentration in points)
            for points in individuals.values()
        ) / len(individuals)
        obs_points = sorted(observed[dose])
        obs_auc = auc(obs_points)
        obs_cmax = max(concentration for _, concentration in obs_points)
        rows.append(
            {
                "molecule": "Pembrolizumab",
                "dose": dose,
                "ap_cmax": pred_cmax / obs_cmax,
                "ap_auc": pred_auc / obs_auc,
                **topdown[dose],
            }
        )
    return rows


def build_docx(rows: list[dict[str, object]]) -> None:
    w_text = BASE["w_text"]
    document = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
        '<w:document xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main">'
        "<w:body>"
        + w_text(
            "Table. Human pembrolizumab Cmax and AUC fold-error by modeling approach",
            bold=True,
            size=20,
        )
        + w_text(
            "Fold-error is reported as predicted/observed. Green indicates within twofold "
            "(0.50–2.00); red indicates outside twofold.",
            size=17,
        )
        + BASE["docx_table"]("Human", rows)
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
    with zipfile.ZipFile(DOCX_OUT, "w", zipfile.ZIP_DEFLATED) as archive:
        archive.writestr("[Content_Types].xml", content_types)
        archive.writestr("_rels/.rels", root_rels)
        archive.writestr("word/document.xml", document)
        archive.writestr("word/styles.xml", styles)
        archive.writestr("word/_rels/document.xml.rels", doc_rels)


def build_xlsx(rows: list[dict[str, object]]) -> None:
    source = Path(__file__).with_name("Fold_Error_Comparison_Tables.xlsx")
    with zipfile.ZipFile(source) as original, zipfile.ZipFile(
        XLSX_OUT, "w", zipfile.ZIP_DEFLATED
    ) as output:
        for name in original.namelist():
            if name == "xl/worksheets/sheet1.xml":
                output.writestr(name, BASE["build_sheet"](rows))
            elif name == "xl/worksheets/sheet2.xml":
                continue
            elif name == "xl/workbook.xml":
                output.writestr(
                    name,
                    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                    '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" '
                    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">'
                    '<sheets><sheet name="Human pembrolizumab" sheetId="1" r:id="rId1"/>'
                    "</sheets></workbook>",
                )
            elif name == "xl/_rels/workbook.xml.rels":
                output.writestr(
                    name,
                    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
                    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
                    '<Relationship Id="rId1" '
                    'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" '
                    'Target="worksheets/sheet1.xml"/>'
                    '<Relationship Id="rId3" '
                    'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" '
                    'Target="styles.xml"/></Relationships>',
                )
            elif name == "[Content_Types].xml":
                text = original.read(name).decode("utf-8")
                text = text.replace(
                    '<Override PartName="/xl/worksheets/sheet2.xml" '
                    'ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
                    "",
                )
                output.writestr(name, text)
            else:
                output.writestr(name, original.read(name))


if __name__ == "__main__":
    data = calculate_rows()
    build_docx(data)
    build_xlsx(data)
    for row in data:
        print(
            f"{row['dose']:g} mg/kg: "
            f"a priori Cmax={row['ap_cmax']:.2f}, AUC={row['ap_auc']:.2f}; "
            f"top-down Cmax={row['td_cmax']:.2f}, AUC={row['td_auc']:.2f}"
        )
    print(DOCX_OUT)
    print(XLSX_OUT)

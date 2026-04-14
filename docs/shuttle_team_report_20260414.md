# FLUXNET Shuttle Data Quality Report — 2026-04-14

**To:** support@fluxnet.org  
**From:** David J.P. Moore, University of Arizona (EcosystemEcologyLab)  
**Re:** 106 sites with all-missing NEE in FLUXNET Annual Paper 2026 dataset  

---

## Summary

During quality control of the FLUXNET Annual Paper 2026 dataset (672 sites, downloaded
via FLUXNET Shuttle v0.3.7, snapshot 2026-04-14), we identified 106 sites where
`NEE_VUT_REF` is `-9999` for every year-row in the FLUXMET YY product. For a subset of
these, `NEE_CUT_REF` is also `-9999` throughout.

**This appears to be a source data issue**: the FLUXMET YY files were correctly packaged
and distributed by the Shuttle. The pipeline reads them without error. The `-9999` fill
values indicate that the ONEFlux processing chain ran but produced no valid u*-filtered
NEE estimate for these sites. We are reporting these to support@fluxnet.org for routing
to the relevant data contributors.

---

## Verification

For a sample of 8 sites (RU-NeF, US-TLR, US-CS6, CN-SnB, US-Lin, US-Sag, CA-PB1,
US-YK1), we confirmed:

- FLUXMET YY CSV file exists and was extracted correctly
- `NEE_VUT_REF` column is present in the CSV (except US-YK1, which has only `NEE_CUT_REF`)
- Every row value is `-9999`; `NEE_VUT_REF_QC` is also `-9999` throughout
- `flux_data_raw_yy.rds` contains the FLUXMET rows for these sites, confirming correct ingest

The `-9999` values are not introduced by our pipeline.

---

## Affected Sites (106 total)

Sorted by data hub, then site ID. `first_year` and `last_year` are from the Shuttle
snapshot and reflect the FLUXMET data span, not the ERA5 span.

| site_id | data_hub | igbp | first_year | last_year | notes |
|---|---|---|---|---|---|
| AR-Bal | AmeriFlux | CRO | 2012 | 2013 | |
| BR-CST | AmeriFlux | DNF | 2014 | 2015 | |
| BR-Ma3 | AmeriFlux | CVM | 2008 | 2018 | |
| BR-SM1 | AmeriFlux | CRO | 2015 | 2016 | |
| CA-BOU | AmeriFlux | WET | 2018 | 2020 | |
| CA-CF1 | AmeriFlux | WET | 2007 | 2008 | |
| CA-CF2 | AmeriFlux | WET | 2008 | 2011 | |
| CA-HPC | AmeriFlux | ENF | 2018 | 2018 | |
| CA-LU2 | AmeriFlux | OSH | 2020 | 2023 | |
| CA-MA1 | AmeriFlux | CRO | 2009 | 2011 | |
| CA-MA2 | AmeriFlux | CRO | 2009 | 2011 | |
| CA-MA3 | AmeriFlux | GRA | 2009 | 2011 | |
| CA-PB1 | AmeriFlux | WET | 2016 | 2024 | both VUT and CUT are -9999 |
| CA-SF1 | AmeriFlux | ENF | 2003 | 2006 | |
| CA-SF3 | AmeriFlux | OSH | 2001 | 2006 | |
| CA-SMC | AmeriFlux | ENF | 2018 | 2018 | |
| MX-PMm | AmeriFlux | WET | 2017 | 2018 | |
| US-A74 | AmeriFlux | CRO | 2016 | 2017 | |
| US-ARb | AmeriFlux | GRA | 2005 | 2006 | |
| US-ARc | AmeriFlux | GRA | 2005 | 2006 | |
| US-ASH | AmeriFlux | DBF | 2016 | 2016 | |
| US-ASM | AmeriFlux | DBF | 2016 | 2016 | |
| US-CAK | AmeriFlux | GRA | 2024 | 2025 | |
| US-CLF | AmeriFlux | CVM | 2017 | 2020 | |
| US-CS1 | AmeriFlux | CRO | 2018 | 2019 | |
| US-CS3 | AmeriFlux | CRO | 2019 | 2020 | |
| US-CS4 | AmeriFlux | CRO | 2020 | 2021 | |
| US-CS5 | AmeriFlux | CRO | 2021 | 2021 | |
| US-CS6 | AmeriFlux | CRO | 2022 | 2023 | |
| US-CS8 | AmeriFlux | CRO | 2023 | 2023 | |
| US-EA6 | AmeriFlux | WSA | 2023 | 2023 | |
| US-Fo1 | AmeriFlux | WET | 2013 | 2024 | |
| US-HVs | AmeriFlux | WET | 1995 | 1995 | |
| US-Hn2 | AmeriFlux | GRA | 2015 | 2018 | |
| US-Hn3 | AmeriFlux | OSH | 2017 | 2018 | |
| US-KS1 | AmeriFlux | ENF | 2002 | 2003 | |
| US-KS3 | AmeriFlux | WET | 2018 | 2018 | |
| US-Lin | AmeriFlux | CRO | 2009 | 2010 | |
| US-MZA | AmeriFlux | WET | 2024 | 2025 | |
| US-Me1 | AmeriFlux | ENF | 2004 | 2005 | |
| US-NGB | AmeriFlux | SNO | 2012 | 2023 | |
| US-NGC | AmeriFlux | GRA | 2017 | 2023 | |
| US-NSa | AmeriFlux | CRO | 2023 | 2025 | |
| US-NSb | AmeriFlux | CRO | 2023 | 2025 | |
| US-PFL | AmeriFlux | DBF | 2019 | 2019 | |
| US-PFb | AmeriFlux | ENF | 2019 | 2019 | |
| US-PFc | AmeriFlux | DBF | 2019 | 2019 | |
| US-PFd | AmeriFlux | WET | 2019 | 2019 | |
| US-PFf | AmeriFlux | GRA | 2019 | 2019 | |
| US-PFg | AmeriFlux | ENF | 2019 | 2019 | |
| US-PFh | AmeriFlux | ENF | 2019 | 2019 | |
| US-PFi | AmeriFlux | DBF | 2019 | 2019 | |
| US-PFj | AmeriFlux | DBF | 2019 | 2019 | |
| US-PFk | AmeriFlux | DBF | 2019 | 2019 | |
| US-PFm | AmeriFlux | DBF | 2019 | 2019 | |
| US-PFn | AmeriFlux | DBF | 2019 | 2019 | |
| US-PFp | AmeriFlux | DBF | 2019 | 2019 | |
| US-PFq | AmeriFlux | DBF | 2019 | 2019 | |
| US-PFr | AmeriFlux | WET | 2019 | 2019 | |
| US-PFt | AmeriFlux | ENF | 2019 | 2019 | |
| US-PSH | AmeriFlux | DBF | 2016 | 2016 | |
| US-PSL | AmeriFlux | DBF | 2016 | 2016 | |
| US-RC5 | AmeriFlux | CRO | 2013 | 2015 | |
| US-Sag | AmeriFlux | WET | 1996 | 1996 | |
| US-TLR | AmeriFlux | MF | 2024 | 2024 | |
| US-Tw2 | AmeriFlux | CRO | 2012 | 2013 | |
| US-UTD | AmeriFlux | CRO | 2023 | 2024 | |
| US-UTE | AmeriFlux | CRO | 2024 | 2025 | |
| US-UTJ | AmeriFlux | CRO | 2024 | 2025 | |
| US-YK1 | AmeriFlux | WET | 2019 | 2025 | CUT column present but also all -9999 |
| BW-Nxr | ICOS | GRA | 2019 | 2020 | |
| CH-For | ICOS | CRO | 2024 | 2024 | |
| CH-Frk | ICOS | GRA | 2013 | 2014 | |
| CN-Qng | ICOS | DNF | 2022 | 2024 | |
| CN-SnB | ICOS | WET | 2021 | 2021 | |
| DE-Etn | ICOS | DBF | 2024 | 2024 | |
| ES-HdD | ICOS | SAV | 2024 | 2024 | |
| ES-TzM | ICOS | DBF | 2020 | 2024 | |
| GL-NuF | ICOS | WET | 2019 | 2024 | |
| GL-ZaF | ICOS | WET | 2023 | 2024 | |
| GL-ZaH | ICOS | GRA | 2020 | 2024 | |
| IL-RmH | ICOS | EBF | 2015 | 2020 | |
| IT-Niv | ICOS | GRA | 2019 | 2024 | |
| JP-Hc2 | ICOS | CRO | 2003 | 2004 | |
| JP-KaP | ICOS | CRO | 1997 | 1997 | |
| JP-Sb1 | ICOS | WET | 2007 | 2010 | |
| JP-Sb2 | ICOS | WET | 2007 | 2010 | |
| JP-Tkb | ICOS | ENF | 2018 | 2019 | |
| KE-Aq1 | ICOS | CRO | 2019 | 2019 | |
| KE-Aq2 | ICOS | CRO | 2022 | 2024 | |
| KR-TwB | ICOS | DBF | 2024 | 2024 | |
| MN-Udg | ICOS | DNF | 2010 | 2012 | |
| RU-Ege | ICOS | DNF | 2010 | 2018 | |
| RU-Fy3 | ICOS | OSH | 2016 | 2018 | |
| RU-Ha1 | ICOS | GRA | 2002 | 2004 | |
| RU-NeB | ICOS | GRA | 1999 | 2000 | |
| RU-NeC | ICOS | OSH | 2001 | 2006 | |
| RU-NeF | ICOS | DNF | 2000 | 2006 | both VUT and CUT are -9999 |
| RU-Sk2 | ICOS | ENF | 2004 | 2008 | |
| RU-SkP | ICOS | DNF | 2004 | 2014 | |
| RU-Tur | ICOS | DNF | 2004 | 2004 | |
| UK-Ags | ICOS | GRA | 2023 | 2024 | |
| AU-Col | TERN | EBF | 2017 | 2019 | |
| AU-Lox | TERN | DBF | 2008 | 2009 | |
| AU-Wac | TERN | EBF | 2005 | 2008 | |
| AU-Wgt | TERN | GRA | 2024 | 2025 | |

---

## Counts by hub

| data_hub | sites affected |
|---|---|
| AmeriFlux | 70 |
| ICOS | 32 |
| TERN | 4 |
| **Total** | **106** |

---

## What we would like to know

1. Is there a known reason why ONEFlux produces no valid NEE for these sites (e.g., insufficient
   data coverage, u* threshold estimation failure, HH data not submitted)?
2. Is it expected that the FLUXMET YY file is distributed with all-`-9999` NEE columns, or
   should these sites have been excluded from the Shuttle listing?
3. For the US-PF* cluster (14 sites, all single-year 2019): these appear to be a coordinated
   submission from one project — is there a known processing issue with this batch?
4. Will these sites receive updated FLUXMET products in a future Shuttle release?

---

*Generated by the FLUXNET Annual Paper 2026 pipeline. Pipeline version: see `git log` for
commit hash at time of report. Contact: David J.P. Moore, davidjpmoore@arizona.edu.*

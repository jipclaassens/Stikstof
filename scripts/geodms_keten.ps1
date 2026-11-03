# Draait items van de Stikstof-configuratie na elkaar met een vaste GeoDMS-versie, elk in een eigen
# aanroep, en stopt bij een exitcode of een [E]-regel. Logs en een statusbestand komen in
# C:\LocalData\Stikstof\runs\<Naam>.
#
# Een nieuwe BAG-levering (#31): zet Parameters/BAG_file_date in cfg/main.dms op de datum van de map
# BAG_<datum>_History en Export_Date in cfg/main/Analyse/N2000_Analyse.dms op de dag van de export, en draai
#   1. /MaakOntkoppeldeData/Write_IsMogelijkWoningCorporatiePand
#   2. /MaakOntkoppeldeData/Write_FinalMutationTable
#   3. /Analyse/N2000_Analyse/Export
#   4. /Output/Vergunningenspoor/Export (de pandexport voor bag_export.py van de vergunningenketen)
# Stap 2 en 3 lezen wat de stap ervoor schreef, dus elk in een eigen aanroep. Op OVSRV06 kostte dat in
# november 2026 samen ongeveer een half uur. Start een lange run buiten de Claude-sessie, via WMI:
#   Invoke-CimMethod -ClassName Win32_Process -MethodName Create -Arguments @{
#     CommandLine = '"C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe" -NoProfile -ExecutionPolicy Bypass -File "C:\ProjDir\Stikstof\scripts\geodms_keten.ps1" -Naam export_<datum> -Items /MaakOntkoppeldeData/Write_FinalMutationTable,/Analyse/N2000_Analyse/Export'
#     CurrentDirectory = 'C:\ProjDir\Stikstof' }
param([string]$Naam, [string[]]$Items, [string]$GeoDmsVersie = "20.23.1.m")
# Met -File komt een kommalijst als een tekst binnen.
$Items = @($Items | ForEach-Object { $_ -split ',' } | Where-Object { $_ })
$exe = "C:\Program Files\ObjectVision\GeoDms$GeoDmsVersie\GeoDmsRun.exe"
$repo = Split-Path -Parent $PSScriptRoot
$cfg = Join-Path $repo "cfg\main.dms"
$dir = Join-Path "C:\LocalData\Stikstof\runs" $Naam
New-Item -ItemType Directory -Force $dir | Out-Null
$status = Join-Path $dir "status.txt"
Set-Content -Path $status -Value ("gestart " + (Get-Date -Format "yyyy-MM-dd HH:mm:ss") + " pid " + $PID) -Encoding UTF8
Add-Content -Path $status -Value ("commit " + (& git -C $repo rev-parse --short HEAD) + ", GeoDMS $GeoDmsVersie") -Encoding UTF8
try {
  $i = 0
  foreach ($item in $Items) {
    $i++
    $log = Join-Path $dir ("{0:D2}.log" -f $i)
    $con = Join-Path $dir ("{0:D2}.console.txt" -f $i)
    Add-Content -Path $status -Value ("start {0} {1}" -f (Get-Date -Format "HH:mm:ss"), $item) -Encoding UTF8
    $sw = [Diagnostics.Stopwatch]::StartNew()
    & $exe "/L$log" $cfg $item *> $con
    $code = $LASTEXITCODE
    $sw.Stop()
    $fouten = @(Select-String -Path $log -Pattern '\[E\]' -ErrorAction SilentlyContinue).Count
    Add-Content -Path $status -Value ("eind  {0} {1}: exit {2}, [E] {3}, {4} min" -f (Get-Date -Format "HH:mm:ss"), $item, $code, $fouten, [math]::Round($sw.Elapsed.TotalMinutes, 1)) -Encoding UTF8
    if ($code -ne 0 -or $fouten -gt 0) { Add-Content -Path $status -Value "GESTOPT" -Encoding UTF8; exit 1 }
  }
  Add-Content -Path $status -Value "KLAAR" -Encoding UTF8
}
catch {
  Add-Content -Path $status -Value ("FOUT in script: " + $_.Exception.Message) -Encoding UTF8
  Add-Content -Path $status -Value "GESTOPT" -Encoding UTF8
  exit 1
}

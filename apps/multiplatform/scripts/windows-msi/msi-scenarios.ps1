param(
  [string]$Scenario = 'all',
  [string]$NewMsiUrl = '',
  [string]$NewMsiDir = '',
  [string]$OldMsiUrl = '',
  [string]$PatchScript = '',
  [string]$Work = 'C:\msitest'
)

$ErrorActionPreference = 'Stop'
$ProgressPreference = 'SilentlyContinue'
Import-Module (Join-Path $PSScriptRoot 'wingui.psm1')

$Evidence = Join-Path $Work 'evidence'
$Logs = Join-Path $Evidence 'logs'
Initialize-Evidence $Evidence
New-Item -ItemType Directory -Force $Logs | Out-Null

$Desktop = [Environment]::GetFolderPath('Desktop')
if (-not $Desktop -or -not $Desktop.EndsWith('\Desktop')) { throw "Unexpected Desktop path '$Desktop'" }
$UserProfile = $env:USERPROFILE
if (-not $UserProfile -or -not (Test-Path (Join-Path $UserProfile 'AppData\Roaming'))) { throw "Unexpected user profile '$UserProfile'" }
$DataDir = Join-Path $env:APPDATA 'SimpleX'
$ProgramFilesDir = Join-Path $env:ProgramFiles 'SimpleX'
$Setup = 'SimpleX Setup'
$UserFiles = @('My notes.txt', 'Tax return 2025.txt', 'Holiday photos\beach.txt')
$SeededNames = @('My notes.txt', 'Tax return 2025.txt', 'Holiday photos')
$InstalledNames = @('SimpleX.exe', 'app', 'runtime')
$FolderUserFiles = @('My profile notes.txt', '.my-settings\settings.txt')
$FolderUserNames = @('My profile notes.txt', '.my-settings')
$script:Failures = @()

function Write-Check([string]$Name, $Value) {
  Write-Host "CHECK $Name = $Value"
  Add-Content (Join-Path $Evidence 'checks.txt') "$Name = $Value"
}

function Assert-Check([string]$Name, $Actual, $Expected) {
  $verdict = if ("$Actual" -eq "$Expected") { 'PASS' } else { 'FAIL' }
  if ($verdict -eq 'FAIL') { $script:Failures += $Name }
  Write-Check $Name "$Actual (expected $Expected) $verdict"
}

function Invoke-Com($Object, [string]$Method, [object[]]$Arguments) {
  $Object.GetType().InvokeMember($Method, [Reflection.BindingFlags]::InvokeMethod, $null, $Object, $Arguments)
}

function Get-ComProperty($Object, [string]$Name, [object[]]$Arguments) {
  $Object.GetType().InvokeMember($Name, [Reflection.BindingFlags]::GetProperty, $null, $Object, $Arguments)
}

function Open-MsiDatabase([string]$Msi, [int]$Mode) {
  Invoke-Com (New-Object -ComObject WindowsInstaller.Installer) 'OpenDatabase' @($Msi, $Mode)
}

function Invoke-MsiSql($Database, [string]$Sql) {
  $view = Invoke-Com $Database 'OpenView' @($Sql)
  Invoke-Com $view 'Execute' @() | Out-Null
  $values = @()
  if ($Sql.StartsWith('SELECT')) {
    while ($record = Invoke-Com $view 'Fetch' @()) {
      $values += Get-ComProperty $record 'StringData' @(1)
    }
  }
  Invoke-Com $view 'Close' @() | Out-Null
  [System.Runtime.InteropServices.Marshal]::ReleaseComObject($view) | Out-Null
  $values
}

function Read-MsiColumn([string]$Msi, [string]$Sql) {
  $db = Open-MsiDatabase $Msi 0
  $values = Invoke-MsiSql $db $Sql
  Close-MsiHandles
  $values
}

function Close-MsiHandles {
  [GC]::Collect()
  [GC]::WaitForPendingFinalizers()
}

function New-LaterVersionMsi([string]$Msi, [string]$Path) {
  Copy-Item $Msi $Path
  Set-ItemProperty $Path -Name IsReadOnly -Value $false
  $db = Open-MsiDatabase $Path 1
  $version = Invoke-MsiSql $db 'SELECT `Value` FROM `Property` WHERE `Property`=''ProductVersion'''
  $parts = $version.Split('.')
  $later = "$($parts[0]).$($parts[1]).$([int]$parts[2] + 1)"
  $upgradeCode = Invoke-MsiSql $db 'SELECT `Value` FROM `Property` WHERE `Property`=''UpgradeCode'''
  Invoke-MsiSql $db "UPDATE ``Property`` SET ``Value``='$later' WHERE ``Property``='ProductVersion'" | Out-Null
  Invoke-MsiSql $db "UPDATE ``Property`` SET ``Value``='{$([guid]::NewGuid().ToString().ToUpper())}' WHERE ``Property``='ProductCode'" | Out-Null
  Invoke-MsiSql $db 'DELETE FROM `Upgrade`' | Out-Null
  Invoke-MsiSql $db "INSERT INTO ``Upgrade`` (``UpgradeCode``, ``VersionMax``, ``Attributes``, ``ActionProperty``) VALUES ('$upgradeCode', '$later', 257, 'JP_UPGRADABLE_FOUND')" | Out-Null
  Invoke-MsiSql $db "INSERT INTO ``Upgrade`` (``UpgradeCode``, ``VersionMin``, ``Attributes``, ``ActionProperty``) VALUES ('$upgradeCode', '$later', 1, 'JP_DOWNGRADABLE_FOUND')" | Out-Null
  foreach ($table in 'Registry', 'RegLocator') {
    foreach ($key in @(Invoke-MsiSql $db "SELECT ``Key`` FROM ``$table``")) {
      if ($key.EndsWith("\$version")) {
        $laterKey = $key.Substring(0, $key.Length - $version.Length) + $later
        Invoke-MsiSql $db "UPDATE ``$table`` SET ``Key``='$laterKey' WHERE ``Key``='$key'" | Out-Null
      }
    }
  }
  $summary = Get-ComProperty $db 'SummaryInformation' @(20)
  $summary.GetType().InvokeMember('Property', [Reflection.BindingFlags]::SetProperty, $null, $summary, @(9, "{$([guid]::NewGuid().ToString().ToUpper())}")) | Out-Null
  Invoke-Com $summary 'Persist' @() | Out-Null
  Invoke-Com $db 'Commit' @() | Out-Null
  $summary = $null
  $db = $null
  Close-MsiHandles
  $Path
}

function Get-Msi([string]$Url, [string]$Dir, [string]$Name) {
  $path = Join-Path $Work "$Name.msi"
  if ($Dir) {
    $file = Get-ChildItem $Dir -Recurse -Filter *.msi | Select-Object -First 1
    if (-not $file) { throw "No .msi in $Dir" }
    Copy-Item $file.FullName $path
  } else {
    Invoke-WebRequest $Url -OutFile $path
  }
  $path
}

function Describe-Msi([string]$Msi, [string]$Name) {
  $version = Read-MsiColumn $Msi 'SELECT `Value` FROM `Property` WHERE `Property`=''ProductVersion'''
  $code = Read-MsiColumn $Msi 'SELECT `Value` FROM `Property` WHERE `Property`=''ProductCode'''
  $rows = try { @(Read-MsiColumn $Msi 'SELECT `WixRemoveFolderEx` FROM `WixRemoveFolderEx`').Count } catch { 'table absent' }
  Write-Check "$Name MSI version" $version
  Write-Check "$Name MSI WixRemoveFolderEx rows" $rows
  Write-Check "$Name MSI sha256" (Get-FileHash $Msi -Algorithm SHA256).Hash
  $code
}

function Test-UserFiles([string]$Label) {
  foreach ($file in $UserFiles) {
    Assert-Check "$Label - Desktop\$file exists" (Test-Path (Join-Path $Desktop $file)) $true
  }
}

function Reset-State {
  Get-Process -Name SimpleX -ErrorAction SilentlyContinue | Stop-Process -Force
  foreach ($code in $script:ProductCodes) {
    Start-Process msiexec.exe -ArgumentList @('/x', $code, '/qn') -Wait
  }
  $installFolders = @($Desktop, $UserProfile, $env:APPDATA)
  $paths = @($SeededNames | ForEach-Object { Join-Path $Desktop $_ })
  foreach ($folder in $installFolders) {
    $paths += @($InstalledNames + $FolderUserNames | ForEach-Object { Join-Path $folder $_ })
  }
  foreach ($path in $paths) {
    if (Test-Path $path) { Remove-Item -Recurse -Force $path }
  }
  New-Item -ItemType Directory -Force $Desktop | Out-Null
}

function Add-FolderUserFiles([string]$Folder) {
  Set-Content (Join-Path $Folder 'My profile notes.txt') 'A user file next to the install folder contents.'
  New-Item -ItemType Directory -Force (Join-Path $Folder '.my-settings') | Out-Null
  Set-Content (Join-Path $Folder '.my-settings\settings.txt') 'A user folder that sorts before AppData.'
}

function Add-UserFiles {
  Set-Content (Join-Path $Desktop 'My notes.txt') 'Personal notes that must survive uninstalling SimpleX.'
  Set-Content (Join-Path $Desktop 'Tax return 2025.txt') 'A document unrelated to SimpleX.'
  New-Item -ItemType Directory -Force (Join-Path $Desktop 'Holiday photos') | Out-Null
  Set-Content (Join-Path $Desktop 'Holiday photos\beach.txt') 'A file in a user folder.'
}

function Install-WithUi([string]$Msi, [string]$Folder, [string]$Tag, [string]$What) {
  $log = Join-Path $Logs "$Tag-install.log"
  $process = Start-Process msiexec.exe -ArgumentList @('/i', "`"$Msi`"", '/l*v', "`"$log`"") -PassThru
  Invoke-Element (Wait-Button $Setup @('Next') 180)
  Wait-Button $Setup @('Change...') | Out-Null
  if ($Folder) {
    Set-EditText $Setup $Folder | Out-Null
    Start-Sleep -Milliseconds 500
    Save-Screenshot "$What - $Folder typed as the install folder"
  } else {
    Save-Screenshot "$What - default install folder"
  }
  $next = Wait-Button $Setup @('Next')
  $next.SetFocus()
  Start-Sleep -Milliseconds 300
  Invoke-Element $next
  $button = Wait-Button $Setup @('Yes', 'Install')
  if ($button.Current.Name -eq 'Yes') {
    Save-Screenshot "$What - installer warns the folder already exists"
    Invoke-Element $button
    $button = Wait-Button $Setup @('Install')
  }
  Invoke-Element $button
  $finish = Wait-Button $Setup @('Finish') 900
  Save-Screenshot "$What - installer finished"
  Invoke-Element $finish
  $process.WaitForExit()
  Assert-Check "$Tag install exit code" $process.ExitCode 0
  $installDir = Select-String -Path $log -Pattern 'Property\(S\): INSTALLDIR = (.*)' | Select-Object -Last 1
  Write-Check "$Tag INSTALLDIR" $(if ($installDir) { $installDir.Matches[0].Groups[1].Value } else { '<not in log>' })
}

function Uninstall-WithUi([string]$Msi, [string]$Tag, [string]$What) {
  $log = Join-Path $Logs "$Tag-uninstall.log"
  $process = Start-Process msiexec.exe -ArgumentList @('/x', "`"$Msi`"", '/l*v', "`"$log`"") -PassThru
  $yes = Wait-Button 'Windows Installer' @('Yes') 180
  Save-Screenshot "$What - uninstall confirmation"
  Invoke-Element $yes
  $process.WaitForExit()
  Assert-Check "$Tag uninstall exit code" $process.ExitCode 0
}

function Start-App([string]$Exe, [string]$Caption) {
  $app = Start-Process $Exe -PassThru
  Start-Sleep -Seconds 45
  Save-Screenshot $Caption
  $running = -not $app.HasExited
  Get-Process -Name SimpleX -ErrorAction SilentlyContinue | Stop-Process -Force
  Start-Sleep -Seconds 3
  $running
}

function Test-UninstallDesktop {
  Reset-State
  Add-UserFiles
  Show-Folder $Desktop 'Desktop with the user''s own files, before installing SimpleX'
  Install-WithUi $script:NewMsi "$Desktop\" 'uninstall-desktop' 'Install into Desktop'
  Show-Folder $Desktop 'Desktop after installing SimpleX into it'
  Assert-Check 'uninstall-desktop - SimpleX.exe installed in Desktop' (Test-Path (Join-Path $Desktop 'SimpleX.exe')) $true
  Uninstall-WithUi $script:NewMsi 'uninstall-desktop' 'Uninstall from Desktop'
  Show-Folder $Desktop 'Desktop after uninstalling SimpleX'
  Test-UserFiles 'uninstall-desktop after uninstall'
  foreach ($name in $InstalledNames) {
    Assert-Check "uninstall-desktop after uninstall - Desktop\$name exists" (Test-Path (Join-Path $Desktop $name)) $false
  }
}

function Test-Update([string]$Tag, [string]$FromMsi, [string]$ToMsi, [string]$FromWhat, [string]$ToWhat) {
  if (-not $FromMsi -or -not $ToMsi) { throw "$Tag needs two MSIs" }
  Reset-State
  Add-UserFiles
  Show-Folder $Desktop "Desktop with the user's own files, before installing $FromWhat"
  Install-WithUi $FromMsi "$Desktop\" "$Tag-from" "Install $FromWhat into Desktop"
  Show-Folder $Desktop "Desktop after installing $FromWhat into it"
  Install-WithUi $ToMsi '' "$Tag-to" "Update to $ToWhat"
  Show-Folder $Desktop "Desktop after updating to $ToWhat"
  Test-UserFiles "$Tag after update"
  foreach ($name in $InstalledNames) {
    Assert-Check "$Tag after update - Desktop\$name exists" (Test-Path (Join-Path $Desktop $name)) $false
  }
  Assert-Check "$Tag - updated SimpleX.exe in Program Files" (Test-Path (Join-Path $ProgramFilesDir 'SimpleX.exe')) $true
  Assert-Check "$Tag - updated app running after 45 s" (Start-App (Join-Path $ProgramFilesDir 'SimpleX.exe') "SimpleX started after updating to $ToWhat") $true
}

function Test-DefaultFolder {
  Reset-State
  Add-UserFiles
  Install-WithUi $script:NewMsi '' 'default-folder' 'Install into the default folder'
  Show-Folder $ProgramFilesDir 'Program Files SimpleX folder after installing'
  Assert-Check 'default-folder - app running after 45 s' (Start-App (Join-Path $ProgramFilesDir 'SimpleX.exe') 'SimpleX started from the default install folder') $true
  Uninstall-WithUi $script:NewMsi 'default-folder' 'Uninstall from the default folder'
  Show-Folder $env:ProgramFiles 'Program Files after uninstalling SimpleX'
  Assert-Check 'default-folder after uninstall - Program Files SimpleX folder exists' (Test-Path $ProgramFilesDir) $false
  Test-UserFiles 'default-folder after uninstall'
}

function Test-DataFolder([string]$Tag, [string]$Folder, [string]$What) {
  Reset-State
  Install-WithUi $script:NewMsi "$Folder\" $Tag "Install into $What"
  Assert-Check "$Tag - app running after 45 s" (Start-App (Join-Path $Folder 'SimpleX.exe') "SimpleX started from $What") $true
  Add-FolderUserFiles $Folder
  $dataFiles = @(Get-ChildItem -Recurse -File $DataDir | ForEach-Object { $_.FullName })
  Write-Check "$Tag - SimpleX data files before uninstall" ($dataFiles -join ', ')
  Assert-Check "$Tag - chat database exists before uninstall" (Test-Path (Join-Path $DataDir 'simplex_v1_chat.db')) $true
  Show-Folder $Folder "$What with SimpleX and the user's files, before uninstalling"
  Show-Folder $DataDir 'SimpleX data folder (database) before uninstalling'
  Uninstall-WithUi $script:NewMsi $Tag "Uninstall from $What"
  Show-Folder $Folder "$What after uninstalling SimpleX"
  Show-Folder $DataDir 'SimpleX data folder (database) after uninstalling'
  foreach ($file in $FolderUserFiles) {
    Assert-Check "$Tag after uninstall - $file in $What exists" (Test-Path (Join-Path $Folder $file)) $true
  }
  foreach ($file in $dataFiles) {
    Assert-Check "$Tag after uninstall - $file exists" (Test-Path $file) $true
  }
  foreach ($name in $InstalledNames) {
    Assert-Check "$Tag after uninstall - $name in $What exists" (Test-Path (Join-Path $Folder $name)) $false
  }
}

Write-Check 'session id' ([System.Diagnostics.Process]::GetCurrentProcess().SessionId)
Write-Check 'explorer running' ([bool](Get-Process explorer -ErrorAction SilentlyContinue))
Write-Check 'screen' ([System.Windows.Forms.SystemInformation]::VirtualScreen)
Write-Check 'EnableLUA' (Get-ItemProperty 'HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System').EnableLUA
Write-Check 'Desktop' $Desktop
Write-Check 'user profile' $UserProfile
Save-Screenshot 'Runner screen at start'

$script:NewMsi = Get-Msi $NewMsiUrl $NewMsiDir 'new'
Write-Check 'direct COM call WindowsInstaller.Installer.OpenDatabase' $(try { $null = (New-Object -ComObject WindowsInstaller.Installer).OpenDatabase($script:NewMsi, 0); 'works' } catch { "fails: $($_.Exception.Message)" })
Close-MsiHandles
if ($PatchScript) {
  & powershell.exe -NoProfile -ExecutionPolicy Bypass -File (Resolve-Path $PatchScript) $script:NewMsi
  Assert-Check 'patch script exit code' $LASTEXITCODE 0
  Write-Check 'patched new MSI - ClearOlderVersionsInstallDirWipe action' ((Read-MsiColumn $script:NewMsi 'SELECT `Target` FROM `CustomAction` WHERE `Action`=''ClearOlderVersionsInstallDirWipe''') -join ' | ')
  Write-Check 'patched new MSI - RemoveExistingProducts sequence' (Read-MsiColumn $script:NewMsi 'SELECT `Sequence` FROM `InstallExecuteSequence` WHERE `Action`=''RemoveExistingProducts''')
}
$script:OldMsi = Get-Msi $OldMsiUrl '' 'old'
$script:ProductCodes = @((Describe-Msi $script:NewMsi 'new'), (Describe-Msi $script:OldMsi 'old'))
$script:LaterMsi = $null
try {
  $script:LaterMsi = New-LaterVersionMsi $script:NewMsi (Join-Path $Work 'later.msi')
  $script:ProductCodes += Describe-Msi $script:LaterMsi 'later'
} catch {
  Write-Check 'later MSI error' $_.Exception.Message
}

$scenarios = [ordered]@{
  'uninstall-desktop' = { Test-UninstallDesktop }
  'update-desktop' = { Test-Update 'update-desktop' $script:OldMsi $script:NewMsi 'the old version' 'the new version' }
  'update-same-build' = { Test-Update 'update-same-build' $script:NewMsi $script:LaterMsi 'the new version' 'a later version of the same build' }
  'default-folder' = { Test-DefaultFolder }
  'profile-folder' = { Test-DataFolder 'profile-folder' $UserProfile 'the user profile folder' }
  'appdata-folder' = { Test-DataFolder 'appdata-folder' $env:APPDATA 'the AppData Roaming folder' }
}
$knownIssues = @('update-desktop')
$selected = switch ($Scenario) {
  'all' { @($scenarios.Keys | Where-Object { $_ -notin $knownIssues }) }
  'everything' { @($scenarios.Keys) }
  default { @($Scenario) }
}
$errors = @()
foreach ($name in $selected) {
  Write-Host "=== SCENARIO $name"
  try {
    & $scenarios[$name]
  } catch {
    Write-Host "SCENARIO $name ERROR: $($_.Exception.Message)"
    Write-Check "$name error" $_.Exception.Message
    $errors += $name
    Get-Process -Name msiexec -ErrorAction SilentlyContinue | Stop-Process -Force -ErrorAction SilentlyContinue
  }
}
Reset-State
Write-Host "FAILED CHECKS: $($script:Failures.Count)"
$script:Failures | ForEach-Object { Write-Host "  FAIL $_" }
if ($errors) { throw "Scenarios with unexpected state: $($errors -join ', ')" }
if ($script:Failures) { throw "$($script:Failures.Count) checks failed" }

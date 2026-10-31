param([string]$Msi)

$ErrorActionPreference = 'Stop'
$msiOpenDatabaseModeTransact = 1

function Invoke-ComMethod($Object, [string]$Method, [object[]]$Arguments) {
  $Object.GetType().InvokeMember($Method, [Reflection.BindingFlags]::InvokeMethod, $null, $Object, $Arguments)
}

$installer = New-Object -ComObject WindowsInstaller.Installer
$database = Invoke-ComMethod $installer 'OpenDatabase' @($Msi, $msiOpenDatabaseModeTransact)
$view = Invoke-ComMethod $database 'OpenView' @('DELETE FROM `WixRemoveFolderEx`')
Invoke-ComMethod $view 'Execute' @()
Invoke-ComMethod $view 'Close' @()
Invoke-ComMethod $database 'Commit' @()

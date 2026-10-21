Add-Type -AssemblyName UIAutomationClient, UIAutomationTypes, System.Windows.Forms, System.Drawing

$script:EvidenceDir = $null
$script:ShotIndex = 0

function Initialize-Evidence([string]$Dir) {
  New-Item -ItemType Directory -Force $Dir | Out-Null
  $script:EvidenceDir = $Dir
  $script:ShotIndex = 0
  Set-Content (Join-Path $Dir 'captions.tsv') "file`tcaption"
}

function Save-Screenshot([string]$Caption) {
  $script:ShotIndex++
  $slug = ($Caption -replace '[^A-Za-z0-9]+', '-').Trim('-').ToLower()
  $name = '{0:D2}-{1}.png' -f $script:ShotIndex, $slug
  try {
    $bounds = [System.Windows.Forms.SystemInformation]::VirtualScreen
    $bmp = New-Object System.Drawing.Bitmap $bounds.Width, $bounds.Height
    $g = [System.Drawing.Graphics]::FromImage($bmp)
    $g.CopyFromScreen($bounds.Left, $bounds.Top, 0, 0, $bmp.Size)
    $bmp.Save((Join-Path $script:EvidenceDir $name), [System.Drawing.Imaging.ImageFormat]::Png)
    $g.Dispose()
    $bmp.Dispose()
    Add-Content (Join-Path $script:EvidenceDir 'captions.tsv') "$name`t$Caption"
    Write-Host "SHOT $name : $Caption"
  } catch {
    Write-Host "SHOT FAILED $name : $Caption : $($_.Exception.Message)"
  }
}

function Get-TopWindow([string]$Title) {
  $cond = New-Object System.Windows.Automation.PropertyCondition([System.Windows.Automation.AutomationElement]::NameProperty, $Title)
  [System.Windows.Automation.AutomationElement]::RootElement.FindFirst([System.Windows.Automation.TreeScope]::Children, $cond)
}

function Find-InWindow([string]$Title, [string]$Name, $ControlType) {
  $window = Get-TopWindow $Title
  if (-not $window) { return $null }
  $cond = New-Object System.Windows.Automation.AndCondition(
    (New-Object System.Windows.Automation.PropertyCondition([System.Windows.Automation.AutomationElement]::NameProperty, $Name)),
    (New-Object System.Windows.Automation.PropertyCondition([System.Windows.Automation.AutomationElement]::ControlTypeProperty, $ControlType)))
  $window.FindFirst([System.Windows.Automation.TreeScope]::Descendants, $cond)
}

function Wait-Window([string]$Title, [int]$TimeoutSec = 60) {
  $deadline = (Get-Date).AddSeconds($TimeoutSec)
  while ((Get-Date) -lt $deadline) {
    $window = Get-TopWindow $Title
    if ($window) { return $window }
    Start-Sleep -Milliseconds 300
  }
  Save-Screenshot "Unexpected state: no window $Title"
  throw "UNEXPECTED STATE: window '$Title' not found within $TimeoutSec s"
}

function Wait-Button([string]$Title, [string[]]$Names, [int]$TimeoutSec = 60) {
  $deadline = (Get-Date).AddSeconds($TimeoutSec)
  while ((Get-Date) -lt $deadline) {
    foreach ($name in $Names) {
      $button = Find-InWindow $Title $name ([System.Windows.Automation.ControlType]::Button)
      if ($button -and $button.Current.IsEnabled) { return $button }
    }
    Start-Sleep -Milliseconds 300
  }
  Save-Screenshot "Unexpected state: no button $($Names -join ' or ') in $Title"
  throw "UNEXPECTED STATE: button '$($Names -join "' or '")' not found in '$Title' within $TimeoutSec s"
}

function Invoke-Element($Element) {
  $Element.GetCurrentPattern([System.Windows.Automation.InvokePattern]::Pattern).Invoke()
}

function Set-EditText([string]$Title, [string]$Text, [int]$TimeoutSec = 60) {
  $deadline = (Get-Date).AddSeconds($TimeoutSec)
  $edit = $null
  while (-not $edit -and (Get-Date) -lt $deadline) {
    $window = Get-TopWindow $Title
    if ($window) {
      $cond = New-Object System.Windows.Automation.PropertyCondition([System.Windows.Automation.AutomationElement]::ControlTypeProperty, [System.Windows.Automation.ControlType]::Edit)
      $edit = $window.FindFirst([System.Windows.Automation.TreeScope]::Descendants, $cond)
    }
    if (-not $edit) { Start-Sleep -Milliseconds 300 }
  }
  if (-not $edit) {
    Save-Screenshot "Unexpected state: no edit box in $Title"
    throw "UNEXPECTED STATE: no edit box in '$Title' within $TimeoutSec s"
  }
  $edit.SetFocus()
  try {
    $edit.GetCurrentPattern([System.Windows.Automation.ValuePattern]::Pattern).SetValue($Text)
  } catch {
    Write-Host "ValuePattern failed ($($_.Exception.Message)), typing instead"
    [System.Windows.Forms.SendKeys]::SendWait('^a')
    [System.Windows.Forms.SendKeys]::SendWait($Text)
  }
  $edit
}

function Close-ExplorerWindows {
  foreach ($window in @((New-Object -ComObject Shell.Application).Windows())) {
    try { $window.Quit() } catch {}
  }
  Start-Sleep -Seconds 1
}

function Show-Folder([string]$Path, [string]$Caption) {
  $listing = if (Test-Path $Path) { (Get-ChildItem -Force $Path | ForEach-Object { $_.Name }) -join ', ' } else { '<folder does not exist>' }
  Write-Host "LISTING $Path : $listing"
  Close-ExplorerWindows
  if (-not (Test-Path $Path)) {
    $Caption = "$Caption (the folder itself no longer exists, its parent is shown)"
    $Path = Split-Path $Path -Parent
  }
  $leaf = Split-Path $Path -Leaf
  Start-Process explorer.exe -ArgumentList "`"$Path`""
  $window = $null
  $deadline = (Get-Date).AddSeconds(20)
  while (-not $window -and (Get-Date) -lt $deadline) {
    $window = Get-TopWindow $leaf
    if (-not $window) { Start-Sleep -Milliseconds 300 }
  }
  if ($window) {
    try {
      $window.GetCurrentPattern([System.Windows.Automation.WindowPattern]::Pattern).SetWindowVisualState([System.Windows.Automation.WindowVisualState]::Maximized)
    } catch {}
    Start-Sleep -Seconds 2
  }
  Save-Screenshot $Caption
  Close-ExplorerWindows
  $unavailable = Find-InWindow 'Location is not available' 'OK' ([System.Windows.Automation.ControlType]::Button)
  if ($unavailable) { Invoke-Element $unavailable }
}

Export-ModuleMember -Function Initialize-Evidence, Save-Screenshot, Get-TopWindow, Find-InWindow, Wait-Window, Wait-Button, Invoke-Element, Set-EditText, Close-ExplorerWindows, Show-Folder

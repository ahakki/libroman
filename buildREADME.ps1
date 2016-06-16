# watch a file changes in the current directory,
# execute all tests when a file is changed or renamed

$watcher = New-Object System.IO.FileSystemWatcher
$watcher.Path = '.\src\Data\Roman\'
$watcher.IncludeSubdirectories = $false
$watcher.EnableRaisingEvents = $false
$watcher.NotifyFilter = [System.IO.NotifyFilters]::LastWrite -bor [System.IO.NotifyFilters]::FileName

while($TRUE){
	$result = $watcher.WaitForChanged([System.IO.WatcherChangeTypes]::Changed -bor [System.IO.WatcherChangeTypes]::Renamed -bOr [System.IO.WatcherChangeTypes]::Created, 1000);
	if($result.TimedOut){
		continue;
	}
    write-host "Change Detected, lets do dis";
    pandoc .\src\Data\Roman\Tutorial.lhs -f markdown+lhs -t markdown_github -o README.md;
    (Get-Content .\README.md).replace('sourceCode', 'haskell') | Set-Content .\README.md

}



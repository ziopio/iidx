$vcvars = "C:\Program Files\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build\vcvars64.bat"
cmd /c "`"$vcvars`" && set" | ForEach-Object {
    $name, $value = $_ -split '=', 2
    Set-Item -Force -Path "Env:\$name" -Value $value
}
Write-Host "MSVC environment initialized."

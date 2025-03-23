.\windows\source.ps1

if (-Not (Test-Path ".\venv")) {
    Write-Host "Installing the python packages to the virtual environment."
    python -m venv .\venv
    & .\venv\Scripts\Activate.ps1
    pip install -r requirements-win.txt
} else {
    & .\venv\Scripts\Activate.ps1
}

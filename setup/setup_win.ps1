Write-Host "This script install all dependencies for the acoustic-calcs project it need to be run in a powershell terminal with admin privileges."

# Ensure that Chocolatey is installed
if (-not (Get-Command choco -ErrorAction SilentlyContinue)) {
    Set-ExecutionPolicy Bypass -Scope Process -Force;
    [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;
    iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
}


# Install Git
choco install git -y

# Install Python
choco install python -y

# Install Docker
choco install docker-desktop -y


choco install ninja -y

choco install mingw -y

Write-Host "Installing PostgreSQL with the password 'test'"
choco install postgresql13 --params '/Password:test' -y

git clone https://github.com/apcave/acoustic-calcs.git


Write-Host "Please restart your computer to continue the setup."
Write-Host "After the restart please run the following command in a powershell terminal from the project root directory:"
Write-Host ".\windows\start_reboot from the root of the project."

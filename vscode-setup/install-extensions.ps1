# Script para instalar extensões VS Code - Ambiente Mainframe COBOL
# Windows PowerShell
# Uso: .\install-extensions.ps1

Write-Host "================================" -ForegroundColor Cyan
Write-Host "Instalando Extensões VS Code" -ForegroundColor Cyan
Write-Host "Ambiente: Mainframe COBOL" -ForegroundColor Cyan
Write-Host "================================" -ForegroundColor Cyan
Write-Host ""

# Verifica se VS Code está instalado
$codePath = Get-Command code -ErrorAction SilentlyContinue
if (-not $codePath) {
    Write-Host "❌ VS Code não encontrado no PATH" -ForegroundColor Red
    Write-Host "Instale VS Code em: https://code.visualstudio.com" -ForegroundColor Yellow
    exit 1
}

Write-Host "✅ VS Code encontrado: $(code --version | Select-Object -First 1)" -ForegroundColor Green
Write-Host ""

# Caminho do arquivo de extensões
$extensionsFile = "$PSScriptRoot\extensions-mainframe.txt"

if (-not (Test-Path $extensionsFile)) {
    Write-Host "❌ Arquivo não encontrado: $extensionsFile" -ForegroundColor Red
    exit 1
}

Write-Host "📦 Lendo extensões de: $extensionsFile" -ForegroundColor Cyan
$extensions = Get-Content $extensionsFile | Where-Object { $_ -ne "" }
$totalExtensions = $extensions.Count

Write-Host "📝 Total de extensões a instalar: $totalExtensions" -ForegroundColor Yellow
Write-Host ""

$installed = 0
$failed = 0

foreach ($extension in $extensions) {
    $extension = $extension.Trim()
    if ($extension -eq "") { continue }
    
    Write-Host "Instalando: $extension" -ForegroundColor Cyan -NoNewline
    $output = & code --install-extension $extension 2>&1
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host " ✅" -ForegroundColor Green
        $installed++
    } else {
        Write-Host " ❌" -ForegroundColor Red
        Write-Host "  Erro: $output" -ForegroundColor Red
        $failed++
    }
}

Write-Host ""
Write-Host "================================" -ForegroundColor Cyan
Write-Host "Resumo da Instalação:" -ForegroundColor Cyan
Write-Host "  ✅ Instaladas: $installed/$totalExtensions" -ForegroundColor Green
if ($failed -gt 0) {
    Write-Host "  ❌ Falharam: $failed/$totalExtensions" -ForegroundColor Red
}
Write-Host "================================" -ForegroundColor Cyan
Write-Host ""

if ($failed -eq 0) {
    Write-Host "🎉 Todas as extensões foram instaladas com sucesso!" -ForegroundColor Green
    Write-Host "Reinicie o VS Code para aplicar as mudanças." -ForegroundColor Yellow
} else {
    Write-Host "⚠️  Algumas extensões falharam. Verifique os erros acima." -ForegroundColor Yellow
}

Write-Host ""
Write-Host "💡 Próximos passos:" -ForegroundColor Cyan
Write-Host "  1. Feche e reabra o VS Code" -ForegroundColor White
Write-Host "  2. Configure as conexões SSH se necessário" -ForegroundColor White
Write-Host "  3. Configure o Endevor Explorer" -ForegroundColor White
Write-Host "  4. Configure o Zowe Explorer" -ForegroundColor White
Write-Host ""

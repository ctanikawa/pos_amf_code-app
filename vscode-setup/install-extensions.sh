#!/bin/bash
# Script para instalar extensões VS Code - Ambiente Mainframe COBOL
# macOS / Linux / WSL
# Uso: chmod +x install-extensions.sh && ./install-extensions.sh

echo "================================"
echo "Instalando Extensões VS Code"
echo "Ambiente: Mainframe COBOL"
echo "================================"
echo ""

# Verifica se VS Code está instalado
if ! command -v code &> /dev/null; then
    echo "❌ VS Code não encontrado no PATH"
    echo "Instale VS Code em: https://code.visualstudio.com"
    exit 1
fi

echo "✅ VS Code encontrado: $(code --version | head -n 1)"
echo ""

# Caminho do arquivo de extensões
EXTENSIONS_FILE="$(dirname "$0")/extensions-mainframe.txt"

if [ ! -f "$EXTENSIONS_FILE" ]; then
    echo "❌ Arquivo não encontrado: $EXTENSIONS_FILE"
    exit 1
fi

echo "📦 Lendo extensões de: $EXTENSIONS_FILE"
TOTAL_EXTENSIONS=$(wc -l < "$EXTENSIONS_FILE")
echo "📝 Total de extensões a instalar: $TOTAL_EXTENSIONS"
echo ""

INSTALLED=0
FAILED=0

while IFS= read -r extension; do
    # Remove espaços em branco
    extension=$(echo "$extension" | xargs)
    
    # Pula linhas vazias e comentários
    if [ -z "$extension" ] || [[ "$extension" =~ ^# ]]; then
        continue
    fi
    
    echo -n "Instalando: $extension ... "
    
    if code --install-extension "$extension" > /dev/null 2>&1; then
        echo "✅"
        ((INSTALLED++))
    else
        echo "❌"
        ((FAILED++))
    fi
done < "$EXTENSIONS_FILE"

echo ""
echo "================================"
echo "Resumo da Instalação:"
echo "  ✅ Instaladas: $INSTALLED/$TOTAL_EXTENSIONS"
if [ $FAILED -gt 0 ]; then
    echo "  ❌ Falharam: $FAILED/$TOTAL_EXTENSIONS"
fi
echo "================================"
echo ""

if [ $FAILED -eq 0 ]; then
    echo "🎉 Todas as extensões foram instaladas com sucesso!"
    echo "Reinicie o VS Code para aplicar as mudanças."
else
    echo "⚠️  Algumas extensões falharam. Verifique os erros acima."
fi

echo ""
echo "💡 Próximos passos:"
echo "  1. Feche e reabra o VS Code"
echo "  2. Configure as conexões SSH se necessário"
echo "  3. Configure o Endevor Explorer"
echo "  4. Configure o Zowe Explorer"
echo ""

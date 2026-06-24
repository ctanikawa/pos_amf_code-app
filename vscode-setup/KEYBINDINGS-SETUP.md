# Configurar Atalho de Teclado para Modo Coluna (Toggle Margins)

## 🎯 Objetivo
Programar uma combinação de teclas para ativar/desativar rapidamente o modo de margens/colunas em COBOL.

---

## ⌨️ Atalhos Disponíveis

| Combinação | Descrição | Recomendação |
|-----------|-----------|--------------|
| **Ctrl+Alt+C** | Toggle Columns (Coluna) | ✅ Recomendado - Fácil de lembrar |
| **Alt+M** | Toggle Margin | Alternativa simples |

---

## 🔧 Como Configurar

### **Opção 1: Usar Arquivo Pronto (Recomendado)**

1. Pressione `Ctrl+Shift+P`
2. Digite: `"Preferences: Open Keyboard Shortcuts (JSON)"`
3. Copie o conteúdo do arquivo `keybindings.json` deste diretório
4. Cole no seu arquivo de keybindings do VS Code
5. Salve e pronto!

---

## ✅ Testando

1. Abra um arquivo `.cob` ou `.cobol`
2. Pressione `Ctrl+Alt+C` (ou `Alt+M`)
3. As linhas verticais devem aparecer/desaparecer

---

## 📝 Notas

- Os atalhos só funcionam em arquivos COBOL (`.cob`, `.cobol`)
- Você pode adicionar ambos os atalhos - usará o que preferir
- Se `Ctrl+Alt+C` já estiver em uso, use `Alt+M` ou customize para outro atalho

---

## 🔗 Referência

- Comando: `coboleditor.togglemargin`
- Extensão: BitLang COBOL (`bitlang.cobol`)
- Atalho padrão: `Alt+Shift+M`

# Configurar Rulers (Linhas Verticais) para COBOL

## 📏 Linhas Verticais no Editor COBOL

As linhas verticais (rulers) marcam as colunas importantes no formato fixo COBOL:

- **Coluna 8**: Início da Área A (DIVISIONS, SECTIONS, PARAGRAPHS)
- **Coluna 72**: Fim da Área B (limite de código útil)
- **Coluna 80**: Fim da linha (cartão perfurado original de 80 caracteres)

---

## 🔧 Como Configurar

### **Opção 1: Via Interface VS Code (Recomendado)**

1. Pressione `Ctrl+,` para abrir Settings
2. Procure por: `"editor.rulers"`
3. Na seção `[cobol]` (ou crie uma), configure:
   ```json
  "editor.rulers": [8, 72, 80],
  "coboleditor.margin": false
   ```
4. Pressione Enter
5. As linhas devem aparecer imediatamente

---

### **Opção 2: Editar settings.json Diretamente**

1. Pressione `Ctrl+Shift+P`
2. Digite: `"Preferences: Open Settings (JSON)"`
3. Procure pela seção `[cobol]` ou crie uma:
   ```json
   "[cobol]": {
    "editor.rulers": [8, 72, 80],
    "coboleditor.margin": false,
     "editor.wordWrap": "off",
     "editor.formatOnSave": false,
     "files.trimTrailingWhitespace": false,
     "editor.tabSize": 4,
     "editor.insertSpaces": false
   }
   ```
4. Salve (Ctrl+S)

---

### **Opção 3: Usar Arquivo Pronto**

Temos um arquivo pronto com todas as configurações: `cobol-rulers-settings.json`

**Para Windows:**
```powershell
# Copiar para a pasta de configurações
Copy-Item "cobol-rulers-settings.json" `
  "$env:APPDATA\Code\User\settings.json" -Force
```

**Para macOS/Linux:**
```bash
# Copiar para a pasta de configurações
cp cobol-rulers-settings.json \
  ~/Library/Application\ Support/Code/User/settings.json
```

---

## 🎨 Customização Avançada

### **Alterar Cor das Linhas**

```json
"[cobol]": {
  "editor.rulers": [8, 72, 80],
  "editor.rulerColor": "#FF6B6B"
}
```

### **Regras Diferentes por Tipo**

```json
"[cobol]": {
  "editor.rulers": [8, 72, 80],
  "coboleditor.margin": false
},
"[cobol-variable-length]": {
  "editor.rulers": [72, 80]
}
```

---

## 📝 Recomendações Adicionais para COBOL

```json
"[cobol]": {
  "editor.rulers": [8, 72, 80],
  "coboleditor.margin": false,
  "editor.wordWrap": "off",
  "editor.formatOnSave": false,
  "files.trimTrailingWhitespace": false,
  "editor.tabSize": 4,
  "editor.insertSpaces": false,
  "editor.renderWhitespace": "selection",
  "editor.fontSize": 11,
  "editor.fontFamily": "Courier New, Consolas, 'Courier New', monospace"
}
```

**Por quê:**
- `wordWrap: off` - Mantém o formato de coluna fixo
- `formatOnSave: false` - Evita reformatação automática
- `trimTrailingWhitespace: false` - Preserva espaços no final
- `insertSpaces: false` - Usa tabs (padrão COBOL)
- `renderWhitespace: selection` - Mostra whitespace apenas na seleção
- `fontSize: 11` - Melhor para código em coluna
- `fontFamily` - Fontes monoespaciais

---

## ✅ Verificar Configuração

Depois de configurar:

1. Abra um arquivo `.COB` (como GEC01.COB)
2. Procure pelas linhas verticais nas colunas 8, 12, 72 e 80
3. Elas devem aparecer como linhas cinzas sutis no editor

Se não aparecer:
- Verifique se o arquivo está salvo como `.COB` ou `.cob`
- Reinicie o VS Code
- Verifique se há espaço suficiente no editor para exibir as linhas

---

## 📦 Arquivo de Configuração

O arquivo `cobol-rulers-settings.json` está pronto em `vscode-setup/` com as configurações otimizadas para COBOL.

Bom desenvolvimento COBOL! 🚀

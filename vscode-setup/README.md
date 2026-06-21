# VS Code Setup - Ambiente Mainframe COBOL

Este diretório contém scripts e configurações para restaurar rapidamente o ambiente VS Code otimizado para desenvolvimento COBOL/Mainframe.

## 📋 Conteúdo

- **extensions-mainframe.txt** - Lista de extensões essenciais
- **install-extensions.ps1** - Script de instalação (Windows PowerShell)
- **install-extensions.sh** - Script de instalação (macOS/Linux/WSL)
- **README.md** - Este arquivo

## 🚀 Instalação Rápida

### Windows PowerShell

```powershell
# Abra o PowerShell como Administrador
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
.\install-extensions.ps1
```

### macOS / Linux / WSL

```bash
chmod +x install-extensions.sh
./install-extensions.sh
```

## 📦 Extensões Incluídas

### IBM/COBOL
- `ibm.zopeneditor` - IBM Z Open Editor (Language Server COBOL)
- `bitlang.cobol` - COBOL (Syntax highlighting)

### Broadcom/Code4z (Mainframe)
- `broadcommfd.code4z-extension-pack` - Code4z Foundation
- `broadcommfd.debugger-for-mainframe` - Debugger para z/OS
- `broadcommfd.explorer-for-endevor` - Explorer para Endevor
- `broadcommfd.bridge-for-git-explorer` - Integração Git
- `broadcommfd.jcl-language-support` - Suporte JCL
- `broadcommfd.data-editor-for-mainframe` - Editor de Datasets

### ZOWE (z/OS)
- `zowe.vscode-extension-for-zowe` - Zowe Explorer

### Microsoft (Remote Development)
- `ms-vscode-remote.vscode-remote-extensionpack` - Remote Development Pack
- `ms-vscode-remote.remote-ssh` - Remote SSH
- `ms-vscode.remote-explorer` - Remote Explorer
- `ms-vscode.remote-repositories` - Remote Repositories

### Git/GitHub
- `eamodio.gitlens` - GitLens
- `github.vscode-pull-request-github` - GitHub Pull Requests

## ⚙️ Configuração Adicional

Após instalar as extensões:

### 1. Settings Sync (Recomendado)
```
Ctrl+Shift+P → "Settings Sync: Turn On"
→ Selecione GitHub
→ Sincronize com sua conta
```

### 2. SSH para Mainframe
```
Ctrl+Shift+P → "Remote-SSH: Connect to Host"
→ Adicione sua conexão z/OS/mainframe
```

### 3. Endevor Explorer
- Vá para a aba "Explorer for Endevor"
- Configure sua conexão Endevor

### 4. Zowe Explorer
- Vá para a aba "Zowe Explorer"
- Adicione seu profile z/OS

## 📝 Backup Completo

Para fazer backup completo de suas configurações:

### Windows
```powershell
# Backup de extensões
code --list-extensions > extensions-backup.txt

# Backup de configurações
Copy-Item "$env:APPDATA\Code\User\settings.json" -Destination "settings-backup.json"
Copy-Item "$env:APPDATA\Code\User\keybindings.json" -Destination "keybindings-backup.json"
```

### macOS/Linux
```bash
# Backup de extensões
code --list-extensions > extensions-backup.txt

# Backup de configurações
cp ~/Library/Application\ Support/Code/User/settings.json settings-backup.json
cp ~/Library/Application\ Support/Code/User/keybindings.json keybindings-backup.json
```

## 🔧 Troubleshooting

### "code: command not found"
Adicione VS Code ao PATH:
- **Windows**: Reinstale VS Code e marque "Add to PATH"
- **macOS**: `ln -s "/Applications/Visual Studio Code.app/Contents/Resources/app/bin/code" /usr/local/bin/code`
- **Linux**: VS Code geralmente já está no PATH

### Extensão não instala
```bash
# Limpe o cache de extensões
code --clear-extensions-dir

# Tente reinstalar
code --install-extension <extension-id>
```

### SSH não conecta
- Verifique suas credenciais SSH
- Confirme acesso ao mainframe/z/OS
- Teste manualmente: `ssh usuario@host`

## 📞 Suporte

Para dúvidas sobre as extensões:
- **IBM Z Open Editor**: https://github.com/IBM/zopeneditor
- **Code4z/Broadcom**: https://github.com/BroadcomMRD/code4z
- **Zowe CLI**: https://github.com/zowe/zowe-cli
- **VS Code**: https://code.visualstudio.com/docs

## 📄 Licenças

Todas as extensões incluídas são open-source ou licenças comerciais padrão. Verifique cada extensão para detalhes de licença.

---

**Última atualização**: 2026-06-20  
**Total de extensões**: 15  
**Plataformas suportadas**: Windows, macOS, Linux, WSL

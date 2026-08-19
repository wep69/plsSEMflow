# Checklist Final — plsSEMflow 0.1.0.9000

## Validação Local

- [x] Backends Suggests todos instalados (`skipped: 0` na suíte)
- [x] Suíte: 0 falhas / 0 erros / 0 pulos (25 testes)
- [x] `R CMD check --as-cran`: 0 ERRORs, 0 WARNINGs, 0 NOTEs
- [x] Vinhetas reconstruídas no check (21 vinhetas)
- [x] Bateria numérica congelada registrada
- [x] win-builder submetido; resultado por e-mail (15-30 min)
- [x] macOS: macbuilder retornou 502 (serviço indisponível); CI cobre macOS

## Git e GitHub

- [x] Repositório criado: https://github.com/wep69/plsSEMflow
- [x] Branch main com 2 commits:
  - `41b5356`: Árvore validada
  - `3e4f231`: CI com pak e README atualizado
- [x] Sync sem sobrescrever README.md
- [x] `git status` limpo de detritos
- [x] Maintainer real no DESCRIPTION (walterufpb@yahoo.com.br)
- [x] URL/BugReports no DESCRIPTION

## CI GitHub Actions

- [x] Workflow configurado com matriz:
  - Windows (release, sem vinhetas)
  - macOS (release, sem vinhetas)
  - Ubuntu (devel, sem vinhetas)
  - Ubuntu (release, com vinhetas)
- [x] pak para instalação rápida
- [x] README com instruções pak/remotes

## Higiene CRAN

- [x] Nomes de vinheta começando com letra (v01-, v02-, etc.)
- [x] Rd: linhas de `\usage` ≤ 90 colunas
- [x] S3 methods com `\method{print}{class}` no Rd
- [x] Arquivos R 100% ASCII
- [x] Funções qualificadas (stats::setNames importado)
- [x] Top-level: .github e _pkgdown.yml excluídos via .Rbuildignore
- [x] globalVariables declarados para ggplot2 NSE
- [x] Imports sem grDevices/graphics (removidos)

## Documentação

- [x] NEWS.md atualizado
- [x] README.md com instruções de instalação
- [x] 21 vinhetas (v01 a v21)

## Status Final

**0 ERRORs | 0 WARNINGs | 0 NOTEs**

Pronto para submissão ao CRAN.

# diff-HA

Um script em Haskell feito principalmente para aprofundar conceitos de recurção em uma linguagem puramente funcional. 
O Script lê dois arquivos e mostra as alterações feitas em cada linha, como inserção de caractere, remoção e alteração. Ideal para comparar pequenas diferenças em arquivos de texto.

## Funcionalidades

- Leitura de dois arquivos de texto.
- Comparação linha a linha para identificar:
  - Inserção de caracteres.
  - Remoção de caracteres.
  - Alteração de caracteres.

## Limitations

O script é eficiente para arquivos pequenos ou com poucas modificações. No entanto, para linhas grandes com muitas modificações, a eficiência pode ser comprometida.

## Requisitos

- [GHC (The Glorious Glasgow Haskell Compilation System)](https://www.haskell.org/ghc/) instalado.
- Sistema operacional compatível com execução de scripts Haskell.

## Como Executar

1. Clone este repositório para o seu ambiente local.
2. Navegue até o diretório onde o script foi baixado.
3. Execute o comando abaixo no terminal:

```bash
./diff-HA.exe
```
O repositório possue arquivos .txt já prontos para teste caso necessário.

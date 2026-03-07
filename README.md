# Sudoku em Haskell

Um projeto de Sudoku desenvolvido em Haskell, contendo geração de
tabuleiros, interação por terminal, verificação de solução e testes
automatizados.

------------------------------------------------------------------------

## 🎯 Objetivo

Implementar um jogo completo de Sudoku em Haskell com:

-   Dois modos de jogo (fácil e difícil)
-   Geração automática de tabuleiros
-   Seleção e manipulação de células (A1, B3, etc.)
-   Preencher e remover números
-   Validação de jogadas
-   Verificação automática da solução
-   Testes unitários e de propriedade

------------------------------------------------------------------------

## 🧱 Estrutura do Projeto

    /src
      Main.hs
      Board.hs
      Generator.hs
      Validation.hs
      UI.hs
      Util.hs

    /tests
      BoardTests.hs
      ValidationTests.hs
      GeneratorTests.hs
      MainTests.hs

    README.md
    cabal.project

------------------------------------------------------------------------

## 🚀 Como Executar

### 1. Clonar o repositório e ir para a pasta

    git clone <url>
    cd projeto_plp

### 2. Executar com Cabal

Para rodar o jogo através do interpretador e acessar o menu:

**Instalar Dependências e Compilar:**

```bash
cabal update
cabal build
```

**Executar o Jogo:**

```bash
cabal run
```


### 3. Executar testes

```
cabal test
```



------------------------------------------------------------------------

## 📌 Funcionalidades

### ✔ Geração de tabuleiro

-   **Fácil**: remove menos números, múltiplos caminhos válidos
-   **Difícil**: remoção mais agressiva dos números iniciais

### ✔ Interação

-   Seleção de células via rótulos (A1, B3...)
-   Preencher número
-   Remover número
-   Exibir tabuleiro

### ✔ Validação

-   Checagem de linha, coluna e subgrade
-   Função `isValidMove`
-   Função `checkSolution`

### ✔ Interface

-   Modo texto simples e intuitivo via terminal
-   Menu inicial para escolha do modo de jogo

### ✔ Testes

-   Testes unitários
-   Testes de propriedade

------------------------------------------------------------------------

## 🛠 Tecnologias

-   **Haskell**
-   **Cabal**
-   **HUnit**
-   **QuickCheck**

------------------------------------------------------------------------

## 👥 Equipe (exemplo)

-   Alana Vanessa
-   Júlia Andrade
-   Letícia Luna
-   Lorena Nascimento
-   Lukas Nascimento
------------------------------------------------------------------------

## 📄 Licença

MIT License.

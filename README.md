# Sudoku em Haskell

Um projeto de Sudoku desenvolvido em Haskell, contendo geração de
tabuleiros, interação por terminal, verificação de solução e testes
automatizados.

------------------------------------------------------------------------

## 🎯 Objetivo

Implementar um jogo completo de Sudoku em Haskell com:

-   Dois modos de jogo (fácil e difícil)\
-   Geração automática de tabuleiros\
-   Seleção e manipulação de células (A1, B3, etc.)\
-   Preencher e remover números\
-   Validação de jogadas\
-   Verificação automática da solução\
-   Testes unitários e de propriedade

------------------------------------------------------------------------

## 🧱 Estrutura do Projeto

    /src
      Main.hs
      Board.hs
      Generator.hs
      GameLoop.hs
      Validation.hs
      UI.hs

    /tests
      BoardTests.hs
      ValidationTests.hs
      GeneratorTests.hs

    README.md
    stack.yaml ou cabal.project

------------------------------------------------------------------------

## 🚀 Como Executar

### 1. Clonar o repositório

    git clone <url>
    cd sudoku-haskell

### 2. Executar com Cabal (Modo Interativo)

Para rodar o jogo através do interpretador e acessar o menu:

**Inicie o REPL:**

```bash
cabal repl
```

### 3. Carregar o módulo e iniciar
```bash
import UI
menu
```

### 4. Executar testes
```
cabal repl sudoku-tests
```



------------------------------------------------------------------------

## 📌 Funcionalidades

### ✔ Geração de tabuleiro

-   **Fácil**: remove menos números, múltiplos caminhos válidos\
-   **Difícil**: garante unicidade de solução e remoção mais agressiva

### ✔ Interação

-   Seleção de células via rótulos (A1, B3...)\
-   Preencher número\
-   Remover número\
-   Exibir tabuleiro

### ✔ Validação

-   Checagem de linha, coluna e subgrade\
-   Função `isValidMove`\
-   Função `checkSolution`

### ✔ Interface

-   Modo texto simples e intuitivo via terminal\
-   Menu inicial para escolha do modo de jogo

### ✔ Testes

-   Testes unitários com HUnit\
-   Testes de propriedade com QuickCheck

------------------------------------------------------------------------

## 🛠 Tecnologias

-   **Haskell**
-   **Stack ou Cabal**
-   **HUnit**
-   **QuickCheck**

------------------------------------------------------------------------

## 👥 Equipe (exemplo)

-   Alana\
-   Lorena\
-   Lukas\
-   Julia\
-   Leticia

------------------------------------------------------------------------

## 📄 Licença

MIT License.

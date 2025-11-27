# Projeto: Construção de Grafo a partir de JSON

Este projeto tem como objetivo construir e manipular grafos utilizando **Python** e **NetworkX**, a partir de um arquivo JSON contendo vértices e arestas.

## 🚀 Funcionalidades

- Leitura de um arquivo JSON para criação de grafo.
- Suporte a **dígrafos** (grafos direcionados).
- Cálculo de menor distância entre nós usando **Dijkstra**.
- Estrutura organizada em módulos.
- Validação dos dados de entrada.

## 🗂 Estrutura do Projeto

```
projeto/
│── src/
│   ├── main.py
│   ├── graph_builder.py
│   ├── util/
│   │   └── validator.py
│── data/
│   └── graph.json
│── README.md
```

## 📝 Exemplo de JSON

```json
{
  "edges": [
    ["A", "B", 5],
    ["B", "C", 3],
    ["A", "C", 10]
  ]
}
```

## ▶️ Executando o projeto

Use:

```bash
python3 src/main.py
```

Certifique-se de que o arquivo `graph.json` está na pasta `data/`.

## 🧪 Testes

Execute:

```bash
python3 -m unittest
```

## 📦 Requisitos

- Python 3.8+
- NetworkX

Instale dependências:

```bash
pip install networkx
```

---

Caso queira personalizar este README, é só pedir! 😊

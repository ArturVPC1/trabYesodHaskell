https://trabyesodhaskell.onrender.com
# Agendamento de Salas (Yesod / Haskell)

## 1. Descrição do projeto
Aplicação web monolítica para agendamento de salas de uma escola de música. Permite cadastrar
escolas, instrumentos, professores, alunos e salas, e marcar aulas em um horário e sala
específicos. O endereço da escola é preenchido automaticamente a partir do CEP, consultando a
API pública ViaCEP.

Funcionalidades principais:
- Cadastro de escolas com endereço obtido pelo CEP (ViaCEP)
- Cadastro e listagem de instrumentos
- CRUD de professores (cada um ensina um instrumento) e de alunos
- CRUD de salas (cada uma pertence a uma escola)
- Agendamento de aulas com um ou mais alunos
- Regras de negócio: não há sobreposição de horário para a mesma sala nem para o mesmo professor, e o horário precisa ser válido (início antes do fim, entre 0h e 23h)
- Cancelamento de agendamentos e restrições de exclusão (escola com salas, sala ou professor com agendamentos)
- Mensagens de sucesso/erro e validação dos campos

## 2. Tecnologias utilizadas
- **Framework:** Yesod 1.6 (Haskell), com templates Hamlet
- **Persistência:** Persistent + SQLite (`agendamento.sqlite3`, criado automaticamente)
- **Integração:** http-conduit + aeson (consumo da API ViaCEP)
- **Linguagem/compilador:** GHC 9.4

## 3. Instruções de execução
Dependências (Debian/Ubuntu):
```
sudo apt install ghc libghc-yesod-dev libghc-persistent-sqlite-dev \
  libghc-persistent-template-dev libghc-http-conduit-dev libghc-aeson-dev
```
Compilar e executar:
```
ghc -O0 -o agendamento Main.hs
./agendamento
```
Acesse http://localhost:3000. As tabelas são criadas na primeira execução.

Ordem sugerida de cadastro: Escolas → Instrumentos → Professores → Alunos → Salas → Agendamentos.

## 4. Estrutura do banco de dados
| Tabela | Descrição |
|---|---|
| `escola` | Nome, CEP e endereço (logradouro, bairro, cidade, UF) |
| `instrumento` | Instrumentos ensinados (nome) |
| `professor` | Nome, e-mail e chave estrangeira para `instrumento` |
| `aluno` | Nome, e-mail e telefone |
| `sala` | Nome, capacidade e chave estrangeira para `escola` |
| `agendamento` | Data, horário de início e fim, finalidade, chaves estrangeiras para `sala` e `professor` |
| `agendamento_aluno` | Tabela de ligação N:N entre `agendamento` e `aluno` (chave única por par) |

## 5. Integração com API
- **API:** ViaCEP — `https://viacep.com.br/ws/{CEP}/json/`
- **Finalidade:** ao cadastrar uma escola, o CEP é consultado e os campos logradouro, bairro,
  cidade e UF são gravados no banco. Se o CEP for inválido ou não existir, o cadastro é recusado
  com mensagem de erro.

## 6. Integrantes
- Artur — desenvolvimento completo do projeto (modelos, handlers, templates, integração com API e documentação)

## Rotas principais
| Método | Rota | Finalidade |
|---|---|---|
| GET/POST | `/escolas` | Listar e cadastrar escolas (com consulta ao CEP) |
| POST | `/escolas/{id}/excluir` | Excluir escola (se não tiver salas) |
| GET/POST | `/instrumentos` | Listar e cadastrar instrumentos |
| GET/POST | `/professores` | Listar e cadastrar professores |
| GET/POST | `/professores/{id}/editar` | Exibir e salvar edição de professor |
| POST | `/professores/{id}/excluir` | Excluir professor (se não tiver agendamentos) |
| GET/POST | `/alunos` | Listar e cadastrar alunos |
| GET/POST | `/alunos/{id}/editar` | Exibir e salvar edição de aluno |
| POST | `/alunos/{id}/excluir` | Excluir aluno |
| GET/POST | `/salas` | Listar e cadastrar salas |
| GET/POST | `/salas/{id}/editar` | Exibir e salvar edição de sala |
| POST | `/salas/{id}/excluir` | Excluir sala (se não tiver agendamentos) |
| GET/POST | `/agendamentos` | Listar e criar agendamentos (com verificação de conflito) |
| POST | `/agendamentos/{id}/excluir` | Cancelar agendamento |

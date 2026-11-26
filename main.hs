{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Yesod
import Database.Persist.Sqlite
import Database.Persist.Sql (fromSqlKey, toSqlKey, ToBackendKey)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day)
import Text.Read (readMaybe)
import Data.Char (isDigit)
import Control.Exception (try, SomeException)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (FromJSON(..), withObject, (.:?), (.!=), eitherDecode)
import Network.HTTP.Simple (parseRequest, httpLBS, getResponseBody, Response)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Escola
    nome Text
    cep Text
    logradouro Text
    bairro Text
    cidade Text
    uf Text
    deriving Show

Instrumento
    nome Text
    deriving Show

Professor
    nome Text
    email Text
    instrumentoId InstrumentoId
    deriving Show

Aluno
    nome Text
    email Text
    telefone Text
    deriving Show

Sala
    nome Text
    capacidade Int
    escolaId EscolaId
    deriving Show

Agendamento
    salaId SalaId
    professorId ProfessorId
    dataAula Day
    horaInicio Int
    horaFim Int
    finalidade Text
    deriving Show

AgendamentoAluno
    agendamentoId AgendamentoId
    alunoId AlunoId
    UniqueAgendamentoAluno agendamentoId alunoId
    deriving Show
|]

data App = App
    { appPool :: ConnectionPool
    }

mkYesod "App" [parseRoutes|
/                         HomeR             GET
/escolas                  EscolasR          GET POST
/escolas/#EscolaId/excluir EscolaExcluirR   POST
/instrumentos             InstrumentosR     GET POST
/professores              ProfessoresR      GET POST
/professores/#ProfessorId/editar  ProfessorEditarR  GET POST
/professores/#ProfessorId/excluir ProfessorExcluirR POST
/alunos                   AlunosR           GET POST
/alunos/#AlunoId/editar   AlunoEditarR      GET POST
/alunos/#AlunoId/excluir  AlunoExcluirR     POST
/salas                    SalasR            GET POST
/salas/#SalaId/editar     SalaEditarR       GET POST
/salas/#SalaId/excluir    SalaExcluirR      POST
/agendamentos             AgendamentosR     GET POST
/agendamentos/#AgendamentoId/excluir AgendamentoExcluirR POST
|]

instance Yesod App where
    approot = ApprootRelative

    defaultLayout widget = do
        pc <- widgetToPageContent widget
        mmsg <- getMessage
        withUrlRenderer [hamlet|
$doctype 5
<html>
    <head>
        <meta charset=utf-8>
        <meta name=viewport content="width=device-width, initial-scale=1">
        <title>#{pageTitle pc}
        ^{pageHead pc}
    <body>
        <nav>
            <a href=@{HomeR}>Início
            \ | <a href=@{EscolasR}>Escolas
            \ | <a href=@{InstrumentosR}>Instrumentos
            \ | <a href=@{ProfessoresR}>Professores
            \ | <a href=@{AlunosR}>Alunos
            \ | <a href=@{SalasR}>Salas
            \ | <a href=@{AgendamentosR}>Agendamentos
        <main>
            $maybe msg <- mmsg
                <p .mensagem>#{msg}
            ^{pageBody pc}
|]

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action (appPool master)

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Agendamento de Salas"
    [whamlet|
        <h1>Agendamento de Salas
        <p>Sistema de agendamento de salas para a escola de música.
    |]

campo :: Text -> Handler (Maybe Text)
campo nome = do
    valor <- runInputPost (iopt textField nome)
    return $ case T.strip <$> valor of
        Just t | not (T.null t) -> Just t
        _ -> Nothing

chaveDe :: ToBackendKey SqlBackend a => Int64 -> Key a
chaveDe = toSqlKey

msgRedirect :: Text -> Route App -> Handler a
msgRedirect m r = setMessage (toHtml m) >> redirect r

data ViaCepResp = ViaCepResp
    { vcErro       :: Bool
    , vcLogradouro :: Text
    , vcBairro     :: Text
    , vcLocalidade :: Text
    , vcUf         :: Text
    }

instance FromJSON ViaCepResp where
    parseJSON = withObject "ViaCepResp" $ \o -> ViaCepResp
        <$> (o .:? "erro" .!= False)
        <*> (o .:? "logradouro" .!= "")
        <*> (o .:? "bairro" .!= "")
        <*> (o .:? "localidade" .!= "")
        <*> (o .:? "uf" .!= "")

buscarCep :: Text -> Handler (Either Text ViaCepResp)
buscarCep cep
    | T.length digitos /= 8 = return (Left "CEP inválido: informe 8 dígitos.")
    | otherwise = do
        resultado <- liftIO (try (httpLBS =<< parseRequest url)
                       :: IO (Either SomeException (Response BL.ByteString)))
        return $ case resultado of
            Left _ -> Left "Não foi possível consultar o CEP agora."
            Right resp -> case eitherDecode (getResponseBody resp) of
                Left _ -> Left "Resposta inválida do serviço de CEP."
                Right r | vcErro r -> Left "CEP não encontrado."
                        | otherwise -> Right r
  where
    digitos = T.filter isDigit cep
    url = "https://viacep.com.br/ws/" ++ T.unpack digitos ++ "/json/"

getEscolasR :: Handler Html
getEscolasR = do
    escolas <- runDB $ selectList [] [Asc EscolaNome]
    defaultLayout $ do
        setTitle "Escolas"
        [whamlet|
            <h1>Escolas
            <p>O endereço é preenchido automaticamente a partir do CEP informado.
            <table>
                <tr><th>Nome<th>CEP<th>Endereço<th>Ações
                $forall Entity eid e <- escolas
                    <tr>
                        <td>#{escolaNome e}
                        <td>#{escolaCep e}
                        <td>#{escolaLogradouro e}, #{escolaBairro e}, #{escolaCidade e}/#{escolaUf e}
                        <td>
                            <form method=post action=@{EscolaExcluirR eid} .inline>
                                <button>Excluir
            <h2>Cadastrar escola
            <form method=post action=@{EscolasR}>
                <label>Nome <input name=nome required>
                <label>CEP <input name=cep required placeholder="00000-000">
                <button>Cadastrar
        |]

postEscolasR :: Handler Html
postEscolasR = do
    mnome <- campo "nome"
    mcep <- campo "cep"
    case (mnome, mcep) of
        (Just nome, Just cep) -> do
            eend <- buscarCep cep
            case eend of
                Left erro -> msgRedirect erro EscolasR
                Right e -> do
                    runDB $ insert_ (Escola nome (T.filter isDigit cep)
                                        (vcLogradouro e) (vcBairro e)
                                        (vcLocalidade e) (vcUf e))
                    msgRedirect "Escola cadastrada." EscolasR
        _ -> msgRedirect "Preencha todos os campos." EscolasR

postEscolaExcluirR :: EscolaId -> Handler Html
postEscolaExcluirR eid = do
    temSalas <- runDB $ count [SalaEscolaId ==. eid]
    if temSalas > 0
        then msgRedirect "Não é possível excluir: a escola possui salas." EscolasR
        else do
            runDB $ delete eid
            msgRedirect "Escola excluída." EscolasR

getInstrumentosR :: Handler Html
getInstrumentosR = do
    instrs <- runDB $ selectList [] [Asc InstrumentoNome]
    defaultLayout $ do
        setTitle "Instrumentos"
        [whamlet|
            <h1>Instrumentos
            <ul>
                $forall Entity _ i <- instrs
                    <li>#{instrumentoNome i}
            <h2>Cadastrar instrumento
            <form method=post action=@{InstrumentosR}>
                <input name=nome required placeholder="Ex.: Violão">
                <button>Cadastrar
        |]

postInstrumentosR :: Handler Html
postInstrumentosR = do
    mnome <- campo "nome"
    case mnome of
        Nothing -> msgRedirect "Informe o nome do instrumento." InstrumentosR
        Just nome -> do
            runDB $ insert_ (Instrumento nome)
            msgRedirect "Instrumento cadastrado." InstrumentosR

professoresComInstrumento :: Handler [(ProfessorId, Professor, Text)]
professoresComInstrumento = do
    ps <- runDB $ selectList [] [Asc ProfessorNome]
    mapM (\(Entity pid p) -> do
            i <- runDB $ get404 (professorInstrumentoId p)
            return (pid, p, instrumentoNome i)) ps

lerProfessor :: Handler (Maybe (Text, Text, InstrumentoId))
lerProfessor = do
    mnome <- campo "nome"
    memail <- campo "email"
    minst <- runInputPost (iopt intField "instrumento")
    return $ case (mnome, memail, minst) of
        (Just n, Just e, Just i) -> Just (n, e, chaveDe (fromIntegral i))
        _ -> Nothing

formProfessor :: Route App -> Maybe Professor -> [Entity Instrumento] -> Widget
formProfessor acao mp instrs = [whamlet|
    <form method=post action=@{acao}>
        <label>Nome <input name=nome required value=#{maybe "" professorNome mp}>
        <label>E-mail <input name=email type=email required value=#{maybe "" professorEmail mp}>
        <label>Instrumento
            <select name=instrumento required>
                $forall Entity iid i <- instrs
                    <option value=#{show (fromSqlKey iid)} :isI iid:selected>#{instrumentoNome i}
        <button>Salvar
|]
  where
    isI iid = maybe False (\p -> professorInstrumentoId p == iid) mp

getProfessoresR :: Handler Html
getProfessoresR = do
    lista <- professoresComInstrumento
    instrs <- runDB $ selectList [] [Asc InstrumentoNome]
    defaultLayout $ do
        setTitle "Professores"
        [whamlet|
            <h1>Professores
            <table>
                <tr><th>Nome<th>E-mail<th>Instrumento<th>Ações
                $forall (pid, p, iNome) <- lista
                    <tr>
                        <td>#{professorNome p}
                        <td>#{professorEmail p}
                        <td>#{iNome}
                        <td>
                            <a href=@{ProfessorEditarR pid}>Editar
                            <form method=post action=@{ProfessorExcluirR pid} .inline>
                                <button>Excluir
            <h2>Cadastrar professor
            ^{formProfessor ProfessoresR Nothing instrs}
        |]

postProfessoresR :: Handler Html
postProfessoresR = do
    dados <- lerProfessor
    case dados of
        Nothing -> msgRedirect "Preencha todos os campos." ProfessoresR
        Just (n, e, i) -> do
            runDB $ insert_ (Professor n e i)
            msgRedirect "Professor cadastrado." ProfessoresR

getProfessorEditarR :: ProfessorId -> Handler Html
getProfessorEditarR pid = do
    p <- runDB $ get404 pid
    instrs <- runDB $ selectList [] [Asc InstrumentoNome]
    defaultLayout $ do
        setTitle "Editar professor"
        [whamlet|
            <h1>Editar professor
            ^{formProfessor (ProfessorEditarR pid) (Just p) instrs}
        |]

postProfessorEditarR :: ProfessorId -> Handler Html
postProfessorEditarR pid = do
    _ <- runDB $ get404 pid
    dados <- lerProfessor
    case dados of
        Nothing -> msgRedirect "Preencha todos os campos." (ProfessorEditarR pid)
        Just (n, e, i) -> do
            runDB $ update pid [ProfessorNome =. n, ProfessorEmail =. e, ProfessorInstrumentoId =. i]
            msgRedirect "Professor atualizado." ProfessoresR

postProfessorExcluirR :: ProfessorId -> Handler Html
postProfessorExcluirR pid = do
    temAgendamentos <- runDB $ count [AgendamentoProfessorId ==. pid]
    if temAgendamentos > 0
        then msgRedirect "Não é possível excluir: o professor possui agendamentos." ProfessoresR
        else do
            runDB $ delete pid
            msgRedirect "Professor excluído." ProfessoresR

lerAluno :: Handler (Maybe (Text, Text, Text))
lerAluno = do
    mnome <- campo "nome"
    memail <- campo "email"
    mtel <- campo "telefone"
    return $ (,,) <$> mnome <*> memail <*> mtel

formAluno :: Route App -> Maybe Aluno -> Widget
formAluno acao ma = [whamlet|
    <form method=post action=@{acao}>
        <label>Nome <input name=nome required value=#{maybe "" alunoNome ma}>
        <label>E-mail <input name=email type=email required value=#{maybe "" alunoEmail ma}>
        <label>Telefone <input name=telefone required value=#{maybe "" alunoTelefone ma}>
        <button>Salvar
|]

getAlunosR :: Handler Html
getAlunosR = do
    alunos <- runDB $ selectList [] [Asc AlunoNome]
    defaultLayout $ do
        setTitle "Alunos"
        [whamlet|
            <h1>Alunos
            <table>
                <tr><th>Nome<th>E-mail<th>Telefone<th>Ações
                $forall Entity aid a <- alunos
                    <tr>
                        <td>#{alunoNome a}
                        <td>#{alunoEmail a}
                        <td>#{alunoTelefone a}
                        <td>
                            <a href=@{AlunoEditarR aid}>Editar
                            <form method=post action=@{AlunoExcluirR aid} .inline>
                                <button>Excluir
            <h2>Cadastrar aluno
            ^{formAluno AlunosR Nothing}
        |]

postAlunosR :: Handler Html
postAlunosR = do
    dados <- lerAluno
    case dados of
        Nothing -> msgRedirect "Preencha todos os campos." AlunosR
        Just (n, e, t) -> do
            runDB $ insert_ (Aluno n e t)
            msgRedirect "Aluno cadastrado." AlunosR

getAlunoEditarR :: AlunoId -> Handler Html
getAlunoEditarR aid = do
    a <- runDB $ get404 aid
    defaultLayout $ do
        setTitle "Editar aluno"
        [whamlet|
            <h1>Editar aluno
            ^{formAluno (AlunoEditarR aid) (Just a)}
        |]

postAlunoEditarR :: AlunoId -> Handler Html
postAlunoEditarR aid = do
    _ <- runDB $ get404 aid
    dados <- lerAluno
    case dados of
        Nothing -> msgRedirect "Preencha todos os campos." (AlunoEditarR aid)
        Just (n, e, t) -> do
            runDB $ update aid [AlunoNome =. n, AlunoEmail =. e, AlunoTelefone =. t]
            msgRedirect "Aluno atualizado." AlunosR

postAlunoExcluirR :: AlunoId -> Handler Html
postAlunoExcluirR aid = do
    runDB $ do
        deleteWhere [AgendamentoAlunoAlunoId ==. aid]
        delete aid
    msgRedirect "Aluno excluído." AlunosR

salasComEscola :: Handler [(SalaId, Sala, Text)]
salasComEscola = do
    ss <- runDB $ selectList [] [Asc SalaNome]
    mapM (\(Entity sid s) -> do
            e <- runDB $ get404 (salaEscolaId s)
            return (sid, s, escolaNome e)) ss

lerSala :: Handler (Maybe (Text, Int, EscolaId))
lerSala = do
    mnome <- campo "nome"
    mcap <- runInputPost (iopt intField "capacidade")
    mesc <- runInputPost (iopt intField "escola")
    return $ case (mnome, mcap, mesc) of
        (Just n, Just c, Just e) | c > 0 -> Just (n, c, chaveDe (fromIntegral e))
        _ -> Nothing

formSala :: Route App -> Maybe Sala -> [Entity Escola] -> Widget
formSala acao ms escolas = [whamlet|
    <form method=post action=@{acao}>
        <label>Nome <input name=nome required value=#{maybe "" salaNome ms}>
        <label>Capacidade <input name=capacidade type=number min=1 required value=#{maybe "" (show . salaCapacidade) ms}>
        <label>Escola
            <select name=escola required>
                $forall Entity eid e <- escolas
                    <option value=#{show (fromSqlKey eid)} :isE eid:selected>#{escolaNome e}
        <button>Salvar
|]
  where
    isE eid = maybe False (\s -> salaEscolaId s == eid) ms

getSalasR :: Handler Html
getSalasR = do
    lista <- salasComEscola
    escolas <- runDB $ selectList [] [Asc EscolaNome]
    defaultLayout $ do
        setTitle "Salas"
        [whamlet|
            <h1>Salas
            <table>
                <tr><th>Nome<th>Capacidade<th>Escola<th>Ações
                $forall (sid, s, eNome) <- lista
                    <tr>
                        <td>#{salaNome s}
                        <td>#{salaCapacidade s}
                        <td>#{eNome}
                        <td>
                            <a href=@{SalaEditarR sid}>Editar
                            <form method=post action=@{SalaExcluirR sid} .inline>
                                <button>Excluir
            <h2>Cadastrar sala
            ^{formSala SalasR Nothing escolas}
        |]

postSalasR :: Handler Html
postSalasR = do
    dados <- lerSala
    case dados of
        Nothing -> msgRedirect "Preencha todos os campos com valores válidos." SalasR
        Just (n, c, e) -> do
            runDB $ insert_ (Sala n c e)
            msgRedirect "Sala cadastrada." SalasR

getSalaEditarR :: SalaId -> Handler Html
getSalaEditarR sid = do
    s <- runDB $ get404 sid
    escolas <- runDB $ selectList [] [Asc EscolaNome]
    defaultLayout $ do
        setTitle "Editar sala"
        [whamlet|
            <h1>Editar sala
            ^{formSala (SalaEditarR sid) (Just s) escolas}
        |]

postSalaEditarR :: SalaId -> Handler Html
postSalaEditarR sid = do
    _ <- runDB $ get404 sid
    dados <- lerSala
    case dados of
        Nothing -> msgRedirect "Preencha todos os campos com valores válidos." (SalaEditarR sid)
        Just (n, c, e) -> do
            runDB $ update sid [SalaNome =. n, SalaCapacidade =. c, SalaEscolaId =. e]
            msgRedirect "Sala atualizada." SalasR

postSalaExcluirR :: SalaId -> Handler Html
postSalaExcluirR sid = do
    temAgendamentos <- runDB $ count [AgendamentoSalaId ==. sid]
    if temAgendamentos > 0
        then msgRedirect "Não é possível excluir: a sala possui agendamentos." SalasR
        else do
            runDB $ delete sid
            msgRedirect "Sala excluída." SalasR

data AgendamentoView = AgendamentoView
    { avId         :: AgendamentoId
    , avSala       :: Text
    , avProfessor  :: Text
    , avData       :: Day
    , avInicio     :: Int
    , avFim        :: Int
    , avFinalidade :: Text
    , avAlunos     :: [Text]
    }

montarAgendamento :: Entity Agendamento -> Handler AgendamentoView
montarAgendamento (Entity aid a) = runDB $ do
    s <- get404 (agendamentoSalaId a)
    p <- get404 (agendamentoProfessorId a)
    vinculos <- selectList [AgendamentoAlunoAgendamentoId ==. aid] []
    alunos <- mapM (get404 . agendamentoAlunoAlunoId . entityVal) vinculos
    return AgendamentoView
        { avId = aid
        , avSala = salaNome s
        , avProfessor = professorNome p
        , avData = agendamentoDataAula a
        , avInicio = agendamentoHoraInicio a
        , avFim = agendamentoHoraFim a
        , avFinalidade = agendamentoFinalidade a
        , avAlunos = map alunoNome alunos
        }

lerAgendamento :: Handler (Maybe (SalaId, ProfessorId, Day, Int, Int, Text, [AlunoId]))
lerAgendamento = do
    msala <- runInputPost (iopt intField "sala")
    mprof <- runInputPost (iopt intField "professor")
    mdata <- runInputPost (iopt dayField "data")
    mini  <- runInputPost (iopt intField "inicio")
    mfim  <- runInputPost (iopt intField "fim")
    mfin  <- campo "finalidade"
    alunosTxt <- lookupPostParams "alunos"
    let alunos = map chaveDe $ mapMaybe (readMaybe . T.unpack) alunosTxt
    return $ case (msala, mprof, mdata, mini, mfim, mfin) of
        (Just s, Just p, Just d, Just i, Just f, Just fin) | not (null alunos) ->
            Just (chaveDe (fromIntegral s), chaveDe (fromIntegral p), d, i, f, fin, alunos)
        _ -> Nothing

temConflito :: SalaId -> ProfessorId -> Day -> Int -> Int -> Handler Bool
temConflito sid pid d ini fim = runDB $ do
    porSala <- count [ AgendamentoSalaId ==. sid, AgendamentoDataAula ==. d
                     , AgendamentoHoraInicio <. fim, AgendamentoHoraFim >. ini ]
    porProf <- count [ AgendamentoProfessorId ==. pid, AgendamentoDataAula ==. d
                     , AgendamentoHoraInicio <. fim, AgendamentoHoraFim >. ini ]
    return (porSala + porProf > 0)

formAgendamento :: [Entity Sala] -> [Entity Professor] -> [Entity Aluno] -> Widget
formAgendamento salas profs alunos = [whamlet|
    <form method=post action=@{AgendamentosR}>
        <label>Sala
            <select name=sala required>
                $forall Entity sid s <- salas
                    <option value=#{show (fromSqlKey sid)}>#{salaNome s}
        <label>Professor
            <select name=professor required>
                $forall Entity pid p <- profs
                    <option value=#{show (fromSqlKey pid)}>#{professorNome p}
        <label>Data <input name=data type=date required>
        <label>Início (hora, 0 a 23) <input name=inicio type=number min=0 max=23 required>
        <label>Fim (hora, 0 a 23) <input name=fim type=number min=0 max=23 required>
        <label>Finalidade <input name=finalidade required placeholder="Ex.: aula de violão">
        <fieldset>
            <legend>Alunos
            $forall Entity aid a <- alunos
                <label .inline>
                    <input type=checkbox name=alunos value=#{show (fromSqlKey aid)}>
                    #{alunoNome a}
        <button>Agendar
|]

getAgendamentosR :: Handler Html
getAgendamentosR = do
    ags <- runDB $ selectList [] [Asc AgendamentoDataAula, Asc AgendamentoHoraInicio]
    views <- mapM montarAgendamento ags
    salas <- runDB $ selectList [] [Asc SalaNome]
    profs <- runDB $ selectList [] [Asc ProfessorNome]
    alunos <- runDB $ selectList [] [Asc AlunoNome]
    defaultLayout $ do
        setTitle "Agendamentos"
        [whamlet|
            <h1>Agendamentos
            <table>
                <tr><th>Data<th>Horário<th>Sala<th>Professor<th>Alunos<th>Finalidade<th>Ações
                $forall av <- views
                    <tr>
                        <td>#{show (avData av)}
                        <td>#{avInicio av}h às #{avFim av}h
                        <td>#{avSala av}
                        <td>#{avProfessor av}
                        <td>#{T.intercalate ", " (avAlunos av)}
                        <td>#{avFinalidade av}
                        <td>
                            <form method=post action=@{AgendamentoExcluirR (avId av)} .inline>
                                <button>Cancelar
            <h2>Novo agendamento
            ^{formAgendamento salas profs alunos}
        |]

postAgendamentosR :: Handler Html
postAgendamentosR = do
    dados <- lerAgendamento
    case dados of
        Nothing -> msgRedirect "Preencha todos os campos e selecione ao menos um aluno." AgendamentosR
        Just (sid, pid, d, ini, fim, fin, alunosIds)
            | ini < 0 || fim > 23 || ini >= fim ->
                msgRedirect "Horário inválido: o início deve ser anterior ao fim (0 a 23h)." AgendamentosR
            | otherwise -> do
                conflito <- temConflito sid pid d ini fim
                if conflito
                    then msgRedirect "Conflito: a sala ou o professor já possui agendamento neste horário." AgendamentosR
                    else do
                        runDB $ do
                            aid <- insert (Agendamento sid pid d ini fim fin)
                            mapM_ (\alid -> insert (AgendamentoAluno aid alid)) alunosIds
                        msgRedirect "Agendamento criado." AgendamentosR

postAgendamentoExcluirR :: AgendamentoId -> Handler Html
postAgendamentoExcluirR aid = do
    runDB $ do
        deleteWhere [AgendamentoAlunoAgendamentoId ==. aid]
        delete aid
    msgRedirect "Agendamento cancelado." AgendamentosR

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "agendamento.sqlite3" 10 $ \pool -> liftIO $ do
    runSqlPool (runMigration migrateAll) pool
    warp 3000 (App pool)

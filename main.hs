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

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "agendamento.sqlite3" 10 $ \pool -> liftIO $ do
    runSqlPool (runMigration migrateAll) pool
    warp 3000 (App pool)
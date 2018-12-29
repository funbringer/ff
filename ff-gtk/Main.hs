{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Control.Lens (ix, makeClassy_, (%~))
import           Control.Monad (void)
import           Data.Function ((&))
import           Data.Text (Text)
import           Data.Vector (Vector, snoc)
import qualified Data.Vector as Vector
import qualified GI.Gtk as Gtk
import           GI.Gtk.Declarative (Attribute ((:=)), BoxChild (BoxChild), bin,
                                     container, defaultBoxChildProperties,
                                     expand, fill, on, onM, widget)
import           GI.Gtk.Declarative.App.Simple (App (App), AppView,
                                                Transition (Exit, Transition),
                                                run)
import qualified GI.Gtk.Declarative.App.Simple

data Todo = Todo{done :: Bool, text :: Text}
makeClassy_ ''Todo

data State = State{currentText :: Text, todos :: Vector Todo}
makeClassy_ ''State

data Event
    = Closed
    | NewTodoChanged Text
    | NewTodoSubmitted
    | TodoToggled Int

view :: State -> AppView Gtk.Window Event
view State{todos, currentText} = bin Gtk.Window attrs mainWidget where
    attrs =
        [ #title := "TodoGTK+"
        , #heightRequest := 300
        , #widthRequest  := 400
        , on #deleteEvent $ const (True, Closed)
        ]

    mainWidget = container Gtk.Box
        [#orientation := Gtk.OrientationVertical]
        [todoList, newTodoForm]

    todoList =
        BoxChild defaultBoxChildProperties{expand = True, fill = True} $
        container Gtk.Box
            [#orientation := Gtk.OrientationVertical]
            (Vector.imap todoItem todos)

    todoItem :: Int -> Todo -> BoxChild Event
    todoItem i Todo{done, text} =
        bin Gtk.CheckButton [#active := done, on #toggled $ TodoToggled i] $
        widget Gtk.Label
            [ #label := if done then "<s>" <> text <> "</s>" else text
            , #halign := Gtk.AlignStart
            , #useMarkup := True
            ]

    newTodoForm = widget Gtk.Entry
        [ #text := currentText
        , #placeholderText := "What needs to be done?"
        , onM #changed $ fmap NewTodoChanged . Gtk.entryGetText
        , on #activate NewTodoSubmitted
        ]

update :: State -> Event -> Transition State Event
update st@State{currentText} = \case
    Closed -> Exit
    NewTodoChanged text -> Transition st{currentText = text} (pure Nothing)
    NewTodoSubmitted ->
        Transition
            (st{currentText = ""}
                & _todos %~ (`snoc` Todo{text = currentText, done = False}))
            (pure Nothing)
    TodoToggled i ->
        Transition (st & _todos . ix i . _done %~ not) (pure Nothing)

main :: IO ()
main = void $ run App{view, update, initialState, inputs} where
    initialState = State{todos = [], currentText = ""}
    inputs = []

{-# ANN module ("HLint: ignore Unused LANGUAGE pragma" :: String) #-}

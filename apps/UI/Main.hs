{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
-- | Entrypoints for running UI widgets.
module UI.Main where

import           Control.Monad                         (void)
import           Control.Monad.IO.Class                (liftIO)

import qualified Data.Text                             as Text

import qualified GHCJS.DOM                             as GHCJS
import qualified GHCJS.DOM.Document                    as Document
import qualified GHCJS.DOM.Document                    as GHCJS
import qualified GHCJS.DOM.Element                     as Element

import qualified Language.Javascript.JSaddle.WebKitGTK as WebKit

import qualified Reflex.Dom                            as Dom

import qualified System.Directory                      as Directory

-- | A concrete type implementing 'Dom t m' that can be run with the
-- functions in this module.
newtype Runnable a = Runnable { run :: forall x. Dom.Widget x a }

-- | Run the widget as-is.
mainWidget :: Runnable a -> IO ()
mainWidget Runnable { run }  = Dom.mainWidget (void run)

-- | Run the widget, loading a CSS file from the given path.
--
-- Given a relative path, the CSS file will be resolved relative to
-- the working directory of the executable.
--
-- To load a CSS file bundled with a Haskell package, you can add the
-- CSS file to your Cabal @data-files@ section and use the
-- @Paths_project_name@ module that Cabal generates:
--
-- @
-- @
--
-- To load multiple CSS files, you can write an @app.css@ file that
-- imports the others. Using relative paths from @app.css@ will work
-- consistently:
--
-- @
-- /* app.css */
-- @import "./specific.css"
-- @import "./path/something-else.css"
-- @
--
-- To ensure that all the CSS files are bundled and distributed with
-- your package, you should add them all to @data-files@.
withCss :: FilePath -> Runnable a -> IO ()
withCss givenPath Runnable { run } = WebKit.run $
  Dom.withJSContextSingleton \ jsSing -> do
    path <- liftIO $ Directory.canonicalizePath givenPath
    document <- GHCJS.currentDocumentUnchecked
    headElement <- GHCJS.getHeadUnchecked document
    Element.setInnerHTML headElement $
      "<link href=\"" <> Text.pack path <> "\" rel=\"stylesheet\">"
    body <- Document.getBodyUnchecked document
    void $ Dom.attachWidget body jsSing run

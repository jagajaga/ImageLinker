module Main where

import           Control.Monad
import           Control.Monad.IO.Class

import qualified Data.Text                  as Text

import           Graphics.UI.Gtk            as Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as Gtk.Event

import           Filesystem.Path.CurrentOS

import           System.Directory
import           System.Environment
import           System.Exit
import           System.Posix.Files

import           Data.IORef

formats :: [[Char]]
formats = ["jpg", "jpeg", "png"]

getSnd :: [[a]] -> [a]
getSnd [] = []
getSnd (_:[]) = []
getSnd (_:x:[]) = x

hasOneOfExtensions :: [String] -> [Char] -> Bool
hasOneOfExtensions e p = result
    where
        e' = map (\l -> Just (Text.pack l)) e
        p' = decodeString p
        result = (extension p' >>= return . Text.toLower) `elem` e'

getImagesFilesFromDirectory :: String -> IO [String]
getImagesFilesFromDirectory = \p -> getDirContents p >>= filterM (\a -> return $ hasOneOfExtensions formats a)

getDirectories :: String -> IO [String]
getDirectories = \p -> getDirContents p >>= filterM doesDirectoryExist >>= \s -> if s == [] then return [] else mapM (\a -> return $ a ++ "/") s

getDirContents :: String -> IO [String]
getDirContents path = getDirectoryContents path >>= filterM (\a -> return (a `notElem` [".", ".."] )) >>= mapM (\a -> return $ path ++ a)

getImagesFromFolders :: String -> IO [String]
getImagesFromFolders path = do
    images <- getImagesFilesFromDirectory path
    directories <- getDirectories path
    toConcat <- mapM getImagesFromFolders directories
    let nextImages = Prelude.concat toConcat
    return $ images ++  nextImages

getFilename :: String -> String
getFilename = encodeString . filename . decodeString

isSymlinkExist :: [String] -> IORef Int -> [Char] -> IO Bool
isSymlinkExist images ref outputFolder = do
    currentPicture <- readIORef ref
    let fileToLook = getFilename (images !! currentPicture)
    doesFileExist $ outputFolder ++ "/" ++ fileToLook

getSymLinkStatus :: [String] -> IORef Int -> [Char] -> IO [Char]
getSymLinkStatus images ref outputFolder = do
    result <- isSymlinkExist images ref outputFolder
    return $ if result then "SYMLINK EXISTS" else "SYMLINK DOESN'T EXISTS"

linkUnLink :: [String] -> IORef Int -> [Char] -> IO ()
linkUnLink images ref outputFolder = do
    result <- isSymlinkExist images ref outputFolder
    currentPicture <- readIORef ref
    let symlinkName = outputFolder ++ "/" ++ (getFilename (images !! currentPicture))
    if not result then createSymbolicLink (images !! currentPicture) symlinkName else removeFile symlinkName

keyPressHandler outputFolder images ref image status sizeW (Gtk.Event.Key {Gtk.Event.eventKeyName = keyName}) = do
  case keyName of
    a | a == "j" || a == "k" -> do
        number <- nextOrPrevPicture images ref image a sizeW
        symLink <- getSymLinkStatus images ref outputFolder
        labelSetText status (images !! number ++ " " ++ symLink)
        return True
    "Escape" ->
        mainQuit >> return True
    "s" -> do
        linkUnLink images ref outputFolder
        symLink <- getSymLinkStatus images ref outputFolder
        number <- readIORef ref
        labelSetText status (images !! number ++ " " ++ symLink)
        return True
    _ -> do
      return True

nextOrPrevPicture images ref image keyName' sizeW = do
    let way = if keyName'  == "j" then (\a -> a + 1) else (\a -> a - 1)
    nextPicture <- readIORef ref >>= return . (\a -> if a == -1 then (length images) - 1 else if a == length images then 0 else a) . way
    writeIORef ref (nextPicture)
    (w, h) <- readIORef sizeW
    pixbuf <- pixbufNewFromFileAtSize (images !! nextPicture) w h
    imageSetFromPixbuf image pixbuf
    return nextPicture

finish :: String -> IO ()
finish dir = do
    cont <- getDirContents dir
    if cont == [".", ".."] then removeDirectory dir else return ()
    mainQuit

resizeAll sizeW image images currentPicture = do
        (w, h) <- eventSize
        cp <- liftIO $ readIORef currentPicture
        liftIO $ writeIORef sizeW (w, h)
        pixbuf <- liftIO $ pixbufNewFromFileAtSize (images !! cp) w h
        liftIO $ imageSetFromPixbuf image pixbuf
        return False

main :: IO ()
main = do
    args <- getArgs
    if args == [] then print "Usage: ImageLinker <Folder Name> [<Folder for links>]" >> exitSuccess else return ()
    initGUI
    window <- windowNew
    set window [windowTitle := "ImageLinker"]
    vbox <- vBoxNew False 0
    path <- getArgs >>= return . (\a -> if last a /= '/' then a ++ "/" else a) . head
    outputFolder <- getArgs >>= return . (\a -> if a == "" then path ++ "Favorites" else a) . getSnd
    createDirectoryIfMissing True outputFolder
    images <- getImagesFromFolders path
    currentPicture <- newIORef 0
    cp <- readIORef currentPicture
    status <- labelNew (Just (images !! cp))
    miscSetAlignment status 0 0
    boxPackStart vbox status PackNatural 0


    (w, h) <- windowGetSize window
    sizeW <- newIORef (w, h)
    pixbuf <- pixbufNewFromFileAtSize (head images) w h
    image <- imageNewFromPixbuf pixbuf
    boxPackStart vbox image PackNatural 0

    onKeyPress window $ keyPressHandler outputFolder images currentPicture image status sizeW
    onDestroy window $ finish outputFolder

    on window configureEvent $ resizeAll sizeW image images currentPicture

    containerAdd window vbox
    widgetShowAll window

    mainGUI


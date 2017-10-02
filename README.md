# Philips Hue API wrapper

Control Philips Hue lights with Haskell.

To get started, import the `Hue` module.

Running `Hue` actions
---------------------

```haskell
runHue "192.168.1.100" $ do
  ls <- map setLight <$> fetchLights
  ls `traverse` (`request` on)
```

Examples
--------

Slowly change a lights' color:
```haskell
colorShift = do
  Just l <- lightWithName "light-on-my-desk"
  forever $ do
    setLight l `request` (on <> increaseHue 1000 <> increaseSaturation 32)
    liftIO $ threadDelay 500000
```

Change the state of multiple lights:
```haskell
allLightsOn = fetchLights >>= traverse 
                              (\l -> request (setLight l) on)
```

```haskell
someLightsOn = do
  ls <- catMaybes <$> traverse lightWithName ["Desk", "Couch", "Kitchen"]
  traverse (\l -> request (setLight l) (on <> brightness 100)) ls 
```

Change the display name:
```haskell
renameTo name newName = do
  Just l <- lightWithName name
  renameLight l `request` LightName (newName)
```

Perform an `alert`:
```haskell
doAlert name = do
  Just l <- lightWithName name
  setLight l `request` alert MultipleCycles 
```

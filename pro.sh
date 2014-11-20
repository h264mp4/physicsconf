cabal clean && cabal configure && cabal build
cp config/settings-pro.yml config/settings.yml
cp dist/build/confroom/confroom .
echo "build done, starting..."
sudo ./confroom Development

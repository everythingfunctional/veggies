#!/bin/sh

if command -v fpm &> /dev/null ; then
  echo "fpm command found."
else
  echo "fpm command not found."
  echo "Downloading and installing fpm."
  git clone git@github.com:fortran-lang/fpm
  cd fpm
  ./install.sh
  cd -
fi

echo "Building make_vegetable_driver."
fpm build --release

install_path=${1:-"${HOME}/.local/bin"}
if [[ ! -d $install_path ]]; then
  mkdir "$install_path"
fi

if [[ -x "${install_path}/make_vegetable_driver" ]]; then
  echo "Overwriting previous make_vegetable_driver installation."
fi
cp build/gfortran_release/app/make_vegetable_driver "$install_path"

if [[ -x "${install_path}/make_vegetable_driver" ]]; then
  echo "Installation successful."
else
  echo "Installation unsuccessful: can't find ${install_path}/make_vegetable_driver"
fi

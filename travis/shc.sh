if [ -n "$COVERAGE" ]; then
  curl -L https://github.com/rubik/stack-hpc-coveralls/releases/download/v0.0.3.0/shc-linux-x64-7.10.2.tar.bz2 | tar -xvj
  ./shc ether regression
fi

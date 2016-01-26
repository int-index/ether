if [ -n "$COVERAGE" ]; then
  COVERAGE_OPT="--coverage"
fi

echo "COVERAGE_OPT=$COVERAGE_OPT"

stack test :regression $COVERAGE_OPT --no-terminal

#!/bin/bash
echo "******************************************************************************"
echo "                              REGRESSION-TESTS                                "
echo "******************************************************************************"
cd tests/regression
for x in `find 18* -name "*.c" | sort`
do
  echo "Testing $x"
  ../../goblint --enable dbg.debug --enable printstats --sets result html $@ $x
  echo "------------------------------------------------------------------------------"
done

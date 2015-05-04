#!/bin/bash

TEST_FILES="src/test/resources/testprograms/lab3/valid/*.kool src/test/resources/inherited-fields.kool"
OUR_CLASSPATH="build/classes/main:lib/cafebabe_2.11-1.2.jar:"
OUR_OUT_CLASSES="build/tmp/our/"
OUR_OUT_TXT="build/tmp/our.txt"
THEIR_CLASSPATH="lib/koolc_2.11-1.3.1.jar:lib/cafebabe_2.11-1.2.jar:"
THEIR_OUT_CLASSES="build/tmp/their/"
THEIR_OUT_TXT="build/tmp/their.txt"

classname() {
  grep "object" "$1" | head -n1 | cut -d " " -f 2
}

echo "Compiling koolc..."
./gradlew --daemon classes
echo "koolc compiled!"
echo

for f in ${TEST_FILES}; do
  echo "$f"

  echo "Clean"
  rm -r "${OUR_OUT_CLASSES}";   mkdir -p "${OUR_OUT_CLASSES}"
  rm -r "${THEIR_OUT_CLASSES}"; mkdir -p "${THEIR_OUT_CLASSES}"

  echo "Compile"
  scala -cp "${OUR_CLASSPATH}"   koolc.Main -d "${OUR_OUT_CLASSES}"   "${f}"
  scala -cp "${THEIR_CLASSPATH}" koolc.Main -d "${THEIR_OUT_CLASSES}" "${f}"

  echo "Run"
  java -cp "${OUR_OUT_CLASSES}"   $(classname "$f") > "${OUR_OUT_TXT}"
  java -cp "${THEIR_OUT_CLASSES}" $(classname "$f") > "${THEIR_OUT_TXT}"

  echo "Compare"
  if diff "${THEIR_OUT_TXT}" "${OUR_OUT_TXT}"; then
    echo " === NO DIFFERENCE ==="
  else
    echo " ========== THERE WERE DIFFERENCES ========== "
    echo "$f"
    exit 1
  fi
  echo
done

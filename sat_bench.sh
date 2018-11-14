rm -f main_script.log

## Given parameters
################################################################################################
#!/bin/bash

for i in "$@"
do
    case $i in
        -to=*|--timeout=*)
            TIMEOUT="${i#*=}"
            shift # past argument=value
            ;;
        -s=*|--solver=*)
            SOLVER="${i#*=}"
            shift # past argument=value
            ;;
        -opt=*|--options=*)
            OPTIONS="${i#*=}"
            shift # past argument=value
            ;;
        --default)
            DEFAULT=YES
            shift # past argument with no value
            ;;
        *)
            # unknown option
            ;;
    esac
done
SOLVER=`pwd`/$SOLVER
BENCH=$1
if [ "${SOLVER}" = "" ] ; then
    echo "`basename $0`: No prover is given!"
    exit 1
else if [ ! -f "${SOLVER}" ] ; then
         echo "`basename $0`: Given prover \"$pr\" not found!"
         exit 1
     else if [ ! -x "${SOLVER}" ] ; then
              echo "$`basename $0`: Given prover \"$pr\" not executable!"
              exit 1
          fi
     fi
fi
echo "COMMAND: ${SOLVER} <file>"
echo "TIMEOUT: ${TIMEOUT}"
echo "OPTIONS: ${OPTIONS}"
echo "BENCH  : ${BENCH}"

## PP options
################################################################################################

cpt=0
COLS=$(tput cols)
limit=""
while [ $cpt -lt $COLS ]; do
    cpt=`expr $cpt + 1`
    limit=`echo -n $limit-`
done

echo -n "$limit"

benchs=`find $BENCH -name "*.cnf*"`
big_total=`printf "$benchs\n" | wc -l`
for smt in $benchs
do
    tput hpa 5
    total=`expr $total + 1`
    echo -n "$total / $big_total"
    ulimit -t $TIMEOUT
    $SOLVER $OPTIONS $smt 2> main_script.log 1> main_script.log
done

case "$1" in
    "sample")
        dune exec bin/$2/$3.exe < bin/$2/sample.txt
    ;;
    "run")
        dune exec bin/$2/$3.exe < bin/$2/input.txt
    ;;
    "create")
        mkdir -p bin/$2
        cp ./template/dune bin/$2/dune
        cp ./template/first.ml bin/$2/first.ml
        cp ./template/second.ml bin/$2/second.ml
        touch bin/$2/sample.txt
        touch bin/$2/input.txt
    ;;
esac

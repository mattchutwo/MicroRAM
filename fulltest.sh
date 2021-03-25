stack build
echo "========================="
echo "Testing memcopy"
stack exec compile -- test/programs/memcpy.ll 500 --from-llvm -oout.cbor
cd ../witness-checker/
cargo run --release -- ../MicroRAM/out.cbor
cd ../MicroRAM/
echo "========================="
echo "Testing grit"
stack exec compile -- ../grit/driver-link.ll 5000 --from-llvm -oout.cbor
cd ../witness-checker/
cargo run --release -- ../MicroRAM/out.cbor
cd ../MicroRAM/
echo "========================="
echo "DONE"

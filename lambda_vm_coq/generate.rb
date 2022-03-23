# Generate the OCaml files to Coq using `coq-of-ocaml`

if ARGV.size < 2 then
  puts "Usage:"
  puts "  ruby generate.rb deku_path vm_path"
  exit(1)
end

deku_path, vm_path = ARGV
full_path = File.join(deku_path, vm_path)

# Generate to Coq by coq-of-ocaml
mli_files = [ ]
generate_files =
  Dir.glob(File.join(full_path, "*.ml")) +   
  mli_files.map {|path| File.join(full_path, path)}
for ocaml_file_name in generate_files.sort do
    command = "cd #{full_path} && coq-of-ocaml #{File.basename(ocaml_file_name)}"
  system(command)
end

# Move the generated Coq file to the folder lambda_vm_coq
coq_files =
  Dir.glob(File.join(full_path, "*.v")) 
for coq_file_name in coq_files.sort do
    command = "cd #{full_path} && mv #{File.basename(coq_file_name)} #{deku_path}/lambda_vm_coq"
  system(command)
end

system("rsync --checksum #{deku_path}/lambda_vm_coq/*.v #{deku_path}/lambda_vm_coq") 


# Generate _CoqProject
system("./configure.sh")
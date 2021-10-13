
# NederScript Compiler

## Why?
This is a compiler for a made up language called NederScript. It is based on Java, but most keywords are translated (sometimes literally) into Dutch. This was a project for the course Programming Paradigms for the Bachelor Technical Computer Science at the University of Twente. This project was done with one partner

## Running the compiler
To run our compiler, you need to run the main function of pp/project/build/NederScriptCompiler.java (from an IDE).

    -You can choose to supply no program arguments, then the compiler will compile the file pp/project/testfile.ns

    -You can choose to supply one program argument, then the compiler will compile that file.
        -Make sure that file is in the folder pp/project! Then supply only the name of the file (with extension)

When running the compiler, the compiler will create a .hs file in the same directory and with the same name as your code file.
The compiler creates an executable file and automatically runs this, but you can also run it manually. The executable will be put in the pp/project/out folder.

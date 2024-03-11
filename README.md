# UCFSBenchmarks

## Prerequisites

```text
(1) Gradle (version >= 7.2)
(2) Antlr V4
(3) Jflex
```

## Build project 

#### Step 1. Clone repository

`git clone https://github.com/cyb3r-b4stard/UCFSBenchmarks.git`

or

`git@github.com:cyb3r-b4stard/UCFSBenchmarks.git`

or

`gh repo clone cyb3r-b4stard/UCFSBenchmarks`

#### Step 2. Go to the project folder

`cd UCFSBenchmarks`

#### Step 3. Go to the folder with lexer files

`cd src/main/kotlin/org/srcgll/lexer`

#### Step 4. Generate lexer file

`jflex Java.x`

#### Step 5. Return to the root folder

`cd ../../../../../..`

#### Step 6. Go to the antlr folder

`cd src/main/kotlin/org/antlr/`

#### Step 7. Generate antlr4 files

`antlr4 Java8.g4`

#### Step 8. Return to the root folder

`cd ../../../../..`

#### Step 9. Build project

`./gradlew JmhJar`

## Execute Benchmakrs

#### Step 1. Go to the build folder with generated Jar file

`cd build/libs`

#### Step 2. Run benchmarks

`java -cp srcgll-jmh.jar org.openjdk.jmh.Main -i 15 -bs 1 -r 0 -wi 5 -w 0 -bm ss -gc true -foe false -f 1 -rff "../results.csv" -tu ns -jvmArgs "-Xmx4096m -Xss4m -XX:+UseG1GC" `

## Results

After executing all benchmarks, results file will be located in the `/build` folder
under name `results.csv`. That file shall be transfered back to Ivan Lomikovskiy
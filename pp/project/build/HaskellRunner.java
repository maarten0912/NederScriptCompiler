package pp.project.build;

import pp.project.generation.SprockellBuilder;

import java.io.File;  // Import the File class
import java.io.FileWriter;   // Import the FileWriter class
import java.io.IOException;  // Import the IOException class to handle errors

public class HaskellRunner {


    public void run(String filename, SprockellBuilder sprockell) {

        try {
            File hs = new File(filename);
            hs.createNewFile();
            FileWriter writer = new FileWriter(filename);
            writer.write(sprockell.getRes());
            writer.close();
        } catch (IOException exc) {
            exc.printStackTrace();
        }



    }
}

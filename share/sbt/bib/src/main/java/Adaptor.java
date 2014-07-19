

import java.io.FileReader;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.TypeDescription;
import org.yaml.snakeyaml.constructor.Constructor;

/**
 * Created by andrei on 18/07/14.
 */
public class Adaptor {
    public static SpecsBean getSB(FileReader fr) {
        Constructor constructor = new Constructor(SpecsBean.class);//Car.class is root
        TypeDescription sbDescription = new TypeDescription(SpecsBean.class);
        constructor.addTypeDescription(sbDescription);
        Yaml yaml = new Yaml(constructor);
        return (SpecsBean) yaml.load(fr);
    }

}

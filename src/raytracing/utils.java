package raytracing;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

import java.awt.*;

public class utils {
    public static float[] sum (float[] a, float[] b) {
        int l = a.length;
        float[] c = new float[l];
        for (int i = 0; i < l; i++) {
            c[i] = a[i] + b[i];
        }
        return c;
    }

    public static float clamp (float x, float mn, float mx) {
        if (mn > x) return mn;
        else if (x > mx) return mx;
        else return x;
    }

    public static int[] imagerow (IFn rc,
                                   int y,
                                   int image_width,
                                   int samples) {
        int[] row = new int[image_width];
        for (int x = 0; x < image_width; x++) {
            float[] pixel_color = {0.0f, 0.0f, 0.0f};
            for (int j = 0; j < samples; j++) {

                float[] px = (float[]) rc.invoke(x, y);
                pixel_color[0] += px[0];
                pixel_color[1] += px[1];
                pixel_color[2] += px[2];
            }
            float scale = 1.0f / samples;
            pixel_color[0] =
                    clamp((float) Math.sqrt(pixel_color[0] * scale), 0.0f
                            , 0.999f);
            pixel_color[1] =
                    clamp((float) Math.sqrt(pixel_color[1] * scale), 0.0f
                    , 0.999f);
            pixel_color[2] =
                    clamp((float) Math.sqrt(pixel_color[2] * scale), 0.0f
                    , 0.999f);
            Color c = new Color(pixel_color[0],
                    pixel_color[1],
                    pixel_color[2]);
            row[x] = c.getRGB();
        }
        return row;
    }
}

//    int[] a = {0, 1, 2};
//    int[] b = {3, 4, 5};
//    int[] c = new int[a.length];
//for (int i = 0; i < a.length; ++i) {
//        c[i] = a[i] + b[i];
//        }

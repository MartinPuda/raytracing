# raytracing

Clojure RayTracing

Ray Tracing in One Weekend:
https://raytracing.github.io/books/RayTracingInOneWeekend.html

Ray Tracing: The Next Week
https://raytracing.github.io/books/RayTracingTheNextWeek.html

## Usage

Run `(-main)` (30 samples per pixel) or `(-main :samples 10)` for different number of samples.

Rendering can take some time.

### Source files hierarchy:
base < noise < texture < ray < materials < rotate, translate < scenes < core

base < rectangles < box

### In progress:
* bvhnode
* performance

## License

Copyright © 2023

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
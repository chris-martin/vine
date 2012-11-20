package vine;

import java.util.List;

import static java.lang.Math.*;
import static java.util.Arrays.asList;
import static vine.Geometry.Side.LEFT;
import static vine.Geometry.Side.RIGHT;

public final class Geometry {
    private Geometry() {
    }

    private static final float EPSILON = 0.000001f;

    private static final float PI = (float) Math.PI;
    private static final float PI2 = PI * 2;
    private static final float HALFPI = PI / 2;
    private static final float NEG_HALFPI = PI / -2;
    private static final float THREE_HALVES_PI = 1.5f * PI;

    private static float mod2pi(float a) {
        return a < 0 ? ((a % PI2) + PI2) : (a % PI2);
    }

    private static float mod2pi(float a, boolean flip) {
        return mod2pi(flip ? a + PI : a);
    }

    private static float elevation(float a) {
        float retval;
        if (a >= NEG_HALFPI && a <= HALFPI) {
            retval = a;
        } else {
            a = mod2pi(a);
            if (a <= HALFPI) {
                retval = a;
            } else if (a >= THREE_HALVES_PI) {
                retval = a - PI2;
            } else {
                retval = PI - a;
            }
        }
        if (retval >= NEG_HALFPI && retval <= HALFPI) {
            return retval;
        } else {
            throw new AssertionError(retval);
        }
    }

    private static int sign(float x) {
        return x == 0 ? 0 : (x < 0 ? -1 : 1);
    }

    /**
     * A point in a Euclidean plane.
     */
    public static interface Vec2 extends Comparable<Vec2> {

        float x();

        float y();

        float ang();

        float mag();

        Vec2 mag(float newMag);

        /**
         * Equivalent to mag(1).
         */
        Vec2 unit();

        Vec2 add(Vec2 o);

        Vec2 sub(Vec2 o);

        Vec2 mult(float factor);

        Vec2 mult(Number factor);

        Vec2 div(float divisor);

        Vec2 div(Number divisor);

        Vec2 rot(float ang);

        Vec2 rot(Number ang);

        Vec2 addX(float o);

        Vec2 addY(float o);

        Vec2 subX(float o);

        Vec2 subY(float o);

        /**
         * This is exactly (0, 0).
         */
        boolean isOrigin();

        Vec2 rot90();

        Vec2 rot180();

        Vec2 rot270();

        /**
         * Scalar (dot) product.
         */
        float dot(Vec2 o);

        /**
         * U x V = U dot rot90(V).
         */
        float cross(Vec2 o);

        /**
         * Raises this 2d vector into three dimensions by adding a zero-value z value.
         */
        Vec3 in3d();

    }

    private static abstract class BaseVec2 implements Vec2 {
        public int compareTo(Vec2 o) {
            return Float.compare(mag(), o.mag());
        }

        public Vec2 add(Vec2 o) {
            return xy(this.x() + o.x(), this.y() + o.y());
        }

        public Vec2 sub(Vec2 o) {
            return xy(this.x() - o.x(), this.y() - o.y());
        }

        public Vec2 mag(float newMag) {
            return angleVec2(ang(), newMag);
        }

        public Vec2 unit() {
            return angleVec2(ang(), 1);
        }

        public Vec2 mult(Number factor) {
            return mult(factor.floatValue());
        }

        public Vec2 div(Number divisor) {
            return div(divisor.floatValue());
        }

        public Vec2 addX(float $) {
            return xy(x() + $, y());
        }

        public Vec2 addY(float $) {
            return xy(x(), y() + $);
        }

        public Vec2 subX(float $) {
            return xy(x() - $, y());
        }

        public Vec2 subY(float $) {
            return xy(x(), y() - $);
        }

        public float dot(Vec2 o) {
            return x() * o.x() + y() * o.y();
        }

        public float cross(Vec2 o) {
            return dot(o.rot90());
        }

        public Vec2 rot(float ang) {
            return angleVec2(ang() + ang, mag());
        }

        public Vec2 rot(Number ang) {
            return rot(ang.floatValue());
        }

        public boolean isOrigin() {
            return false;
        }

        public String toString() {
            return String.format("(%f, %f)", x(), y());
        }
    }

    private static class XY extends BaseVec2 {
        final float x, y;
        float ang, mag;
        boolean hasAng, hasMag;

        XY(float x, float y) {
            this.x = x;
            this.y = y;
        }

        public float x() {
            return x;
        }

        public float y() {
            return y;
        }

        public float ang() {
            if (!hasAng) {
                ang = (float) atan2(y, x);
                hasAng = true;
            }
            return ang;
        }

        public float mag() {
            if (!hasMag) {
                mag = (float) sqrt(pow(x, 2) + pow(y, 2));
                hasMag = true;
            }
            return mag;
        }

        public XY rot180() {
            return new XY(-1 * x, -1 * y);
        }

        public XY rot90() {
            return new XY(-1 * y, x);
        }

        public XY rot270() {
            return new XY(y, -1 * x);
        }

        public Vec2 mult(float f) {
            if (abs(f) < EPSILON) return origin2();
            return xy(f * x, f * y);
        }

        public Vec2 div(float d) {
            return xy(x / d, y / d);
        }

        public Vec3 in3d() {
            return xyz(x, y, 0);
        }
    }

    public static Vec2 xy(float x, float y) {
        return abs(x) < EPSILON && abs(y) < EPSILON ? ORIGIN_2 : new XY(x, y);
    }

    public static Vec2 xy(Number x, Number y) {
        return xy(x.floatValue(), y.floatValue());
    }

    public static Vec2 xy(java.awt.Point p) {
        return new XY(p.x, p.y);
    }

    public static Vec2 xy(java.awt.event.MouseEvent e) {
        return new XY(e.getX(), e.getY());
    }

    private static class Ang2 extends BaseVec2 {
        final float ang, mag;
        float x, y;
        boolean hasXy;

        Ang2(float ang, float mag) {
            this.ang = mod2pi(ang, mag < 0);
            this.mag = abs(mag);
        }

        Ang2(float ang) {
            this.ang = mod2pi(ang);
            mag = 1;
        }

        private void ensureXy() {
            if (hasXy) return;
            x = mag * (float) cos(ang);
            y = mag * (float) sin(ang);
            hasXy = true;
        }

        public float x() {
            ensureXy();
            return x;
        }

        public float y() {
            ensureXy();
            return y;
        }

        public float ang() {
            return ang;
        }

        public float mag() {
            return mag;
        }

        public Vec2 rot180() {
            return angleVec2(ang + PI, mag);
        }

        public Vec2 rot90() {
            return angleVec2(ang + HALFPI, mag);
        }

        public Vec2 rot270() {
            return angleVec2(ang - HALFPI, mag);
        }

        public Vec2 mult(float f) {
            if (abs(f) < EPSILON) return origin2();
            return angleVec2(ang, f * mag);
        }

        public Vec2 div(float d) {
            return angleVec2(ang, mag / d);
        }

        public Vec3 in3d() {
            return azimuthAndElevation(ang, 0, mag);
        }
    }

    public static Vec2 angleVec2(float ang, float mag) {
        return abs(mag) < EPSILON ? ORIGIN_2 : new Ang2(ang, mag);
    }

    public static Vec2 angleVec2(Number ang, Number mag) {
        return angleVec2(ang.floatValue(), mag.floatValue());
    }

    public static Vec2 angleVec2(float ang, Number mag) {
        return angleVec2(ang, mag.floatValue());
    }

    public static Vec2 angleVec2(Number ang, float mag) {
        return angleVec2(ang.floatValue(), mag);
    }

    public static Vec2 angleVec2(float ang) {
        return new Ang2(ang);
    }

    public static Vec2 angleVec2(Number ang) {
        return new Ang2(ang.floatValue());
    }

    private static class Origin2 implements Vec2 {
        public float x() {
            return 0;
        }

        public float y() {
            return 0;
        }

        public float ang() {
            return 0;
        }

        public float mag() {
            return 0;
        }

        public Vec2 mult(float factor) {
            return this;
        }

        public Vec2 div(float divisor) {
            return this;
        }

        public Vec2 rot90() {
            return this;
        }

        public Vec2 rot270() {
            return this;
        }

        public Vec2 rot180() {
            return this;
        }

        public Vec2 add(Vec2 o) {
            return o;
        }

        public Vec2 sub(Vec2 o) {
            return o.mult(-1);
        }

        public Vec2 addX(float $) {
            return xy($, 0);
        }

        public Vec2 addY(float $) {
            return xy(0, $);
        }

        public Vec2 subX(float $) {
            return xy(-$, 0);
        }

        public Vec2 subY(float $) {
            return xy(0, -$);
        }

        public Vec2 mag(float newMag) {
            return this;
        }

        public Vec2 unit() {
            return this;
        }

        public Vec2 mult(Number factor) {
            return this;
        }

        public Vec2 div(Number divisor) {
            return this;
        }

        public float dot(Vec2 o) {
            return 0;
        }

        public float cross(Vec2 o) {
            return 0;
        }

        public Vec2 rot(float ang) {
            return this;
        }

        public Vec2 rot(Number ang) {
            return this;
        }

        public int compareTo(Vec2 o) {
            return Float.compare(0, o.mag());
        }

        public boolean isOrigin() {
            return true;
        }

        public Vec3 in3d() {
            return origin3();
        }

        public String toString() {
            return "(0, 0)";
        }
    }

    private static final Origin2 ORIGIN_2 = new Origin2();

    public static Vec2 origin2() {
        return ORIGIN_2;
    }

    /**
     * A directed line segment in a Euclidean plane.
     */
    public static interface Line2 {

        Vec2 a();

        Vec2 b();

        Vec2 ab();

        float mag();

        float ang();

        Side side(Vec2 p);

        Line2 add(Vec2 offset);

        Line2 sub(Vec2 offset);

        Vec2 midpoint();

        Line2 bisect();

        /**
         * Not equal, but correlated, to "bulge" as defined by Jarek Rossignac.
         */
        float bulge(Vec2 p);

    }

    public static enum Side {
        LEFT(-1), RIGHT(1);
        final int i;

        Side(int i) {
            this.i = i;
        }

        Side opposite() {
            return this == LEFT ? RIGHT : LEFT;
        }
    }

    private static abstract class BaseLine2 implements Line2 {
        public Side side(Vec2 p) {
            return p.sub(a()).cross(b().sub(a())) > 0 ? LEFT : RIGHT;
        }

        public Line2 bisect() {
            return pointAndStep(midpoint(), Geometry.angleVec2(ang()).rot90());
        }

        public float bulge(Vec2 p) {
            Circle2 c = circle(a(), b(), p);
            return c.radius() * side(p).i * side(c.center()).i;
        }
    }

    private static class OriginLine2 extends BaseLine2 {
        final Vec2 b;

        OriginLine2(Vec2 b) {
            this.b = b;
        }

        public Vec2 a() {
            return ORIGIN_2;
        }

        public Vec2 b() {
            return b;
        }

        public Vec2 ab() {
            return b;
        }

        public float ang() {
            return b.ang();
        }

        public float mag() {
            return b.mag();
        }

        public Line2 add(Vec2 offset) {
            return aToB(offset, b.add(offset));
        }

        public Line2 sub(Vec2 offset) {
            return aToB(offset.mult(-1), b.sub(offset));
        }

        public Vec2 midpoint() {
            return b.div(2);
        }
    }

    public static Line2 oTo2(float ang) {
        return new OriginLine2(Geometry.angleVec2(ang));
    }

    public static Line2 oTo2(Number ang) {
        return new OriginLine2(angleVec2(ang));
    }

    public static Line2 oTo2(Vec2 p) {
        return new OriginLine2(p);
    }

    private static class AtoB2 extends BaseLine2 {

        final Vec2 a, b;
        Vec2 ab;

        AtoB2(Vec2 a, Vec2 b) {
            this.a = a;
            this.b = b;
        }

        AtoB2(Vec2 a, Vec2 b, Vec2 ab) {
            this.a = a;
            this.b = b;
            this.ab = ab;
        }

        public Vec2 a() {
            return a;
        }

        public Vec2 b() {
            return b;
        }

        public Vec2 ab() {
            if (ab == null) ab = b.sub(a);
            return ab;
        }

        public float ang() {
            return ab().ang();
        }

        public float mag() {
            return ab().mag();
        }

        public Line2 add(Vec2 offset) {
            return new AtoB2(offset.add(a), b.add(offset), ab);
        }

        public Line2 sub(Vec2 offset) {
            return new AtoB2(offset.sub(a), b.sub(offset), ab);
        }

        public Vec2 midpoint() {
            return a.add(b).div(2);
        }

        public String toString() {
            return String.format("Line %s to %s", a, b);
        }
    }

    public static Line2 aToB(Vec2 a, Vec2 b) {
        return new AtoB2(a, b);
    }

    private static class PointAndDirection2 extends BaseLine2 {

        final Vec2 a, ab;

        PointAndDirection2(Vec2 a, Vec2 ab) {
            this.a = a;
            this.ab = ab;
        }

        public Vec2 a() {
            return a;
        }

        public Vec2 b() {
            return a.add(ab);
        }

        public Vec2 ab() {
            return ab;
        }

        public float ang() {
            return ab.ang();
        }

        public float mag() {
            return ab.mag();
        }

        public Line2 add(Vec2 offset) {
            return pointAndStep(a.add(offset), ab);
        }

        public Line2 sub(Vec2 offset) {
            return pointAndStep(a.sub(offset), ab);
        }

        public Vec2 midpoint() {
            return ab.div(2).add(a);
        }
    }

    public static Line2 pointAndStep(Vec2 a, Vec2 ab) {
        return new PointAndDirection2(a, ab);
    }

    public static Line2 pointAndStep(Vec2 a, float ang) {
        return new PointAndDirection2(a, Geometry.angleVec2(ang));
    }

    public static Line2 pointAndStep(Vec2 a, Number ang) {
        return new PointAndDirection2(a, angleVec2(ang));
    }

    public static Vec2 intersect(Line2 ab, Line2 cd) {
        Vec2 v1 = ab.a(), v2 = ab.b(), v3 = cd.a(), v4 = cd.b();
        // http://en.wikipedia.org/wiki/Line-line_intersection
        float x1 = v1.x(), y1 = v1.y(), x2 = v2.x(), y2 = v2.y(), x3 = v3.x(), y3 = v3.y(), x4 = v4.x(), y4 = v4.y();
        float d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);
        float x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / d;
        float y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / d;
        return xy(x, y);
    }

    public static boolean overlap(Line2 ab, Line2 cd) {
        Vec2 a = ab.a(), b = ab.b(), c = cd.a(), d = cd.b();
        return ab.side(c) != ab.side(d) && cd.side(a) != cd.side(b);
    }

    public static interface Circle2 {

        Vec2 center();

        float radius();

    }

    private static class SimpleCircle2 implements Circle2 {

        final Vec2 center;
        final float radius;

        SimpleCircle2(Vec2 center, float radius) {
            this.center = center;
            this.radius = radius;
        }

        public Vec2 center() {
            return center;
        }

        public float radius() {
            return radius;
        }
    }

    public static Circle2 circle(Vec2 center, float radius) {
        return new SimpleCircle2(center, radius);
    }

    public static Circle2 circle(Vec2 center, Number radius) {
        return new SimpleCircle2(center, radius.floatValue());
    }

    private static class TriangleCircle2 implements Circle2 {
        final Vec2[] vs;
        Vec2 center;
        float radius;
        boolean hasRadius;

        TriangleCircle2(Vec2[] vs) {
            this.vs = vs;
        }

        public Vec2 center() {
            if (center == null) _center();
            return center;
        }

        void _center() {
            center = intersect(aToB(vs[0], vs[1]).bisect(), aToB(vs[1], vs[2]).bisect());
        }

        public float radius() {
            if (!hasRadius) _radius();
            return radius;
        }

        void _radius() {
            radius = center().sub(vs[0]).mag();
            hasRadius = true;
        }
    }

    public static Circle2 circle(Vec2 a, Vec2 b, Vec2 c) {
        return new TriangleCircle2(new Vec2[]{a, b, c});
    }

    /**
     * 0, 1, or 2 intersections.
     */
    public static Vec2[] intersect(Line2 line, Circle2 circle) {
        // http://mathworld.wolfram.com/Circle-LineIntersection.html
        float r = circle.radius();
        Vec2 cc = circle.center();
        line = line.sub(cc);
        Vec2 a = line.a(), b = line.b(), ab = line.ab();
        float dx = ab.x(), dy = ab.y();
        float dr = (float) sqrt(pow(dx, 2) + pow(dy, 2));
        float D = a.x() * b.y() - b.x() * a.y();
        float q = (float) sqrt(pow(r, 2) * pow(dr, 2) - pow(D, 2));
        if (q < 0) return new Vec2[0];
        float qx = sign(dy) * dx * q, qy = abs(dy) * q;
        float Ddy = D * dy, nDdx = 0 - D * dx;
        if (qx == 0 && qy == 0) return new Vec2[]{xy(Ddy, nDdx)};
        Vec2[] is = new Vec2[]{xy(Ddy + qx, nDdx + qy), xy(Ddy - qx, nDdx - qy)};
        for (int i = 0; i < 2; i++) is[i] = is[i].div(pow(dr, 2)).add(cc);
        return is;
    }


    /**
     * A point in three dimensions.
     */
    public static interface Vec3 extends IsVec3, Comparable<Vec3> {

        float x();

        float y();

        float z();

        float mag();

        float magSquared();

        Vec3 mag(float newMag);

        /**
         * Equivalent to mag(1).
         */
        Vec3 unit();

        Vec3 add(Vec3 o);

        Vec3 sub(Vec3 o);

        Vec3 mult(float factor);

        Vec3 mult(Number factor);

        Vec3 div(float divisor);

        Vec3 div(Number divisor);

        Vec3 addX(float o);

        Vec3 addY(float o);

        Vec3 addZ(float o);

        Vec3 subX(float o);

        Vec3 subY(float o);

        Vec3 subZ(float o);

        /**
         * This is exactly (0, 0, 0).
         */
        boolean isOrigin();

        /**
         * Scalar (dot) product.
         */
        float dot(Vec3 o);

        /**
         * Cross product U X V, normal to both U and V.
         */
        Vec3 cross(Vec3 o);

        /** Rotate 90 degrees in the XY plane. */
        Vec3 rot90xy();

        float azimuth();

        float elevation();

        Vec3 azimuth(float newAzimuth);

        Vec3 elevation(float newElevation);

        Vec2 xy();

        /** An arbitrary orthogonal vector. */
        Vec3 orthog();

    }

    private static abstract class BaseVec3 implements Vec3 {
        public int compareTo(Vec3 o) {
            return Float.compare(mag(), o.mag());
        }

        public Vec3 add(Vec3 o) {
            return xyz(this.x() + o.x(), this.y() + o.y(), this.z() + o.z());
        }

        public Vec3 sub(Vec3 o) {
            return xyz(this.x() - o.x(), this.y() - o.y(), this.z() - o.z());
        }

        public Vec3 mag(float newMag) {
            return unit().mult(newMag);
        }

        public Vec3 unit() {
            return div(mag());
        }

        public Vec3 mult(Number factor) {
            return mult(factor.floatValue());
        }

        public Vec3 div(Number divisor) {
            return div(divisor.floatValue());
        }

        public Vec3 addX(float $) {
            return xyz(x() + $, y(), z());
        }

        public Vec3 addY(float $) {
            return xyz(x(), y() + $, z());
        }

        public Vec3 addZ(float $) {
            return xyz(x(), y(), z() + $);
        }

        public Vec3 subX(float $) {
            return xyz(x() - $, y(), z());
        }

        public Vec3 subY(float $) {
            return xyz(x(), y() - $, z());
        }

        public Vec3 subZ(float $) {
            return xyz(x(), y(), z() - $);
        }

        public float dot(Vec3 o) {
            return x() * o.x() + y() * o.y() + z() * o.z();
        }

        public Vec3 cross(Vec3 o) {
            return xyz(
                y() * o.z() - z() * o.y(),
                z() * o.x() - x() * o.z(),
                x() * o.y() - y() * o.x()
            );
        }

        public boolean isOrigin() {
            return false;
        }

        public Vec3 elevation(float newElevation) {
            return azimuthAndElevation(azimuth(), newElevation, mag());
        }

        public Vec3 azimuth(float newAzimuth) {
            return azimuthAndElevation(newAzimuth, elevation(), mag());
        }

        public Vec3 orthog() {
            return azimuthAndElevation(azimuth(), elevation() + HALFPI, mag());
        }

        public String toString() {
            return String.format("(%f, %f, %f)", x(), y(), z());
        }

        public Vec3 asVec3() {
            return this;
        }
    }

    private static class XYZ extends BaseVec3 {

        final float x, y, z;
        float mag, magSquared, elevation, azimuth;
        boolean hasMag, hasMagSquared, hasElevation, hasAzimuth;

        XYZ(float x, float y, float z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        public float x() {
            return x;
        }

        public float y() {
            return y;
        }

        public float z() {
            return z;
        }

        public float mag() {
            if (!hasMag) {
                mag = (float) sqrt(magSquared());
                hasMag = true;
            }
            return mag;
        }

        public float magSquared() {
            if (!hasMagSquared) {
                magSquared = (float) (pow(x, 2) + pow(y, 2) + pow(z, 2));
                hasMagSquared = true;
            }
            return magSquared;
        }

        public Vec3 mult(float f) {
            if (abs(f) < EPSILON) return origin3();
            return xyz(f * x, f * y, f * z);
        }

        public Vec3 div(float d) {
            return xyz(x / d, y / d, z / d);
        }

        public Vec3 rot90xy() {
            return xyz(-1 * y, x, z);
        }

        public float elevation() {
            if (!hasElevation) {
                elevation = Geometry.elevation((float) asin(z / mag()));
                hasElevation = true;
            }
            return elevation;
        }

        public float azimuth() {
            if (!hasAzimuth) {
                azimuth = Geometry.mod2pi((float) atan2(y, x));
                hasAzimuth = true;
            }
            return azimuth;
        }

        public Vec2 xy() {
            return Geometry.xy(x, y);
        }
    }

    public static Vec3 xyz(float x, float y, float z) {
        return abs(x) < EPSILON && abs(y) < EPSILON && abs(z) < EPSILON ? ORIGIN_3 : new XYZ(x, y, z);
    }

    public static Vec3 xyz(Number x, Number y, Number z) {
        return xyz(x.floatValue(), y.floatValue(), z.floatValue());
    }

    public static Vec3 xyz(float[] array) {
        return xyz(array[0], array[1], array[2]);
    }

    public static Vec3 xyz(double[] array) {
        return xyz(array[0], array[1], array[2]);
    }

    public static Vec3 xyz(Number[] array) {
        return xyz(array[0], array[1], array[2]);
    }

    private static class Origin3 implements Vec3 {

        public float x() {
            return 0;
        }

        public float y() {
            return 0;
        }

        public float z() {
            return 0;
        }

        public float mag() {
            return 0;
        }

        public float magSquared() {
            return 0;
        }

        public Vec3 mult(float factor) {
            return this;
        }

        public Vec3 div(float divisor) {
            return this;
        }

        public Vec3 add(Vec3 o) {
            return o;
        }

        public Vec3 sub(Vec3 o) {
            return o.mult(-1);
        }

        public Vec3 addX(float $) {
            return xyz($, 0, 0);
        }

        public Vec3 addY(float $) {
            return xyz(0, $, 0);
        }

        public Vec3 addZ(float $) {
            return xyz(0, 0, $);
        }

        public Vec3 subX(float $) {
            return xyz(-$, 0, 0);
        }

        public Vec3 subY(float $) {
            return xyz(0, -$, 0);
        }

        public Vec3 subZ(float $) {
            return xyz(0, 0, -$);
        }

        public Vec3 mag(float newMag) {
            return this;
        }

        public Vec3 unit() {
            return this;
        }

        public Vec3 mult(Number factor) {
            return this;
        }

        public Vec3 div(Number divisor) {
            return this;
        }

        public float dot(Vec3 o) {
            return 0;
        }

        public Vec3 cross(Vec3 o) {
            return this;
        }

        public Vec3 rot90xy() {
            return this;
        }

        public int compareTo(Vec3 o) {
            return Float.compare(0, o.mag());
        }

        public boolean isOrigin() {
            return true;
        }

        public float azimuth() {
            return 0;
        }

        public float elevation() {
            return 0;
        }

        public Vec3 azimuth(float newAzimuth) {
            return this;
        }

        public Vec3 elevation(float newElevation) {
            return this;
        }

        public Vec2 xy() {
            return origin2();
        }

        public Vec3 orthog() {
            return this;
        }

        public String toString() {
            return "(0, 0, 0)";
        }

        public Vec3 asVec3() {
            return this;
        }
    }

    private static final Origin3 ORIGIN_3 = new Origin3();

    public static Vec3 origin3() {
        return ORIGIN_3;
    }

    public static float distance(Vec2 a, Vec2 b) {
        return a.sub(b).mag();
    }

    public static float distance(Vec3 a, Vec3 b) {
        return a.sub(b).mag();
    }

    public static float distance(IsVec3 a, IsVec3 b) {
        return a.asVec3().sub(b.asVec3()).mag();
    }

    public static Vec2 midpoint(Vec2 a, Vec2 b) {
        return a.add(b).div(2);
    }

    public static Vec3 midpoint(Vec3 a, Vec3 b) {
        return a.add(b).div(2);
    }


    /**
     * A directed line segment in three dimensional space.
     */
    public static interface Line3 {

        Vec3 a();

        Vec3 b();

        Vec3 ab();

        float mag();

        Line3 add(Vec3 offset);

        Line3 sub(Vec3 offset);

        Vec3 midpoint();

        /**
         * Changes A without affecting B.
         */
        Line3 a(Vec3 a);

        /**
         * Changes B without affecting A.
         */
        Line3 b(Vec3 b);

        /**
         * Changes A without affecting AB.
         */
        Line3 aShift(Vec3 a);

        /**
         * Changes B without affecting AB.
         */
        Line3 bShift(Vec3 b);

        /**
         * Changes AB without affecting A.
         */
        Line3 ab(Vec3 ab);

        /**
         * Elevation of AB.
         */
        float elevation();

        /**
         * Azimuth of AB.
         */
        float azimuth();

        /**
         * An arbitrary line, passing through A, orthogonal to this line.
         */
        Line3 aOrthog();

        /**
         * An arbitrary line, passing through B, orthogonal to this line.
         */
        Line3 bOrthog();

        Line3 reverse();

        /**
         * Move b such that the magnitude of ab is multiplied.
         */
        Line3 mult(float factor);

    }

    private static abstract class BaseLine3 implements Line3 {

        public float mag() {
            return ab().mag();
        }

        public Line3 a(Vec3 a) {
            return aToB(a, b());
        }

        public Line3 b(Vec3 b) {
            return aToB(a(), b);
        }

        public Line3 aShift(Vec3 a) {
            return pointAndStep(a, ab());
        }

        public Line3 bShift(Vec3 b) {
            Vec3 ab = ab();
            return new AtoB3(b.sub(ab), b, ab);
        }

        public Line3 ab(Vec3 ab) {
            return pointAndStep(a(), ab);
        }

        public float elevation() {
            return ab().elevation();
        }

        public float azimuth() {
            return ab().azimuth();
        }

        public Line3 aOrthog() {
            return pointAndStep(a(), ab().orthog());
        }

        public Line3 bOrthog() {
            return reverse().aOrthog();
        }

        public Line3 mult(float factor) {
            return pointAndStep(a(), ab().mult(factor));
        }

        public String toString() {
            return String.format("%s -> %s", a(), b());
        }
    }

    private static class AtoB3 extends BaseLine3 {
        final Vec3 a, b;
        Vec3 ab;

        AtoB3(Vec3 a, Vec3 b) {
            this.a = a;
            this.b = b;
        }

        AtoB3(Vec3 a, Vec3 b, Vec3 ab) {
            this.a = a;
            this.b = b;
            this.ab = ab;
        }

        public Vec3 a() {
            return a;
        }

        public Vec3 b() {
            return b;
        }

        public Vec3 ab() {
            if (ab == null) ab = b.sub(a);
            return ab;
        }

        public Line3 add(Vec3 offset) {
            return new AtoB3(offset.add(a), b.add(offset), ab);
        }

        public Line3 sub(Vec3 offset) {
            return new AtoB3(a.sub(offset), b.sub(offset), ab);
        }

        public Line3 reverse() {
            return new AtoB3(b, a, ab == null ? null : ab.mult(-1));
        }

        public Vec3 midpoint() {
            return a.add(b).div(2);
        }

        public String toString() {
            return String.format("Line %s to %s", a, b);
        }
    }

    public static Line3 aToB(IsVec3 a, IsVec3 b) {
        return aToB(a.asVec3(), b.asVec3());
    }

    public static Line3 aToB(Vec3 a, Vec3 b) {
        return new AtoB3(a, b);
    }

    private static class AzimuthAndElevation extends BaseVec3 {

        final float azimuth, elevation, mag;
        Vec3 xyz;

        private AzimuthAndElevation(float azimuth, float elevation, float mag) {
            this.azimuth = mod2pi(azimuth, mag < 0);
            this.elevation = Geometry.elevation(elevation) * sign(mag);
            this.mag = abs(mag);
        }

        public float azimuth() {
            return azimuth;
        }

        public float elevation() {
            return elevation;
        }

        Vec3 xyz() {
            if (xyz == null) {
                double cosAzimuth = cos(azimuth), cosElevation = cos(elevation),
                    sinAzimuth = sin(azimuth), sinElevation = sin(elevation);
                xyz = Geometry.xyz(
                    cosElevation * cosAzimuth,
                    cosElevation * sinAzimuth,
                    sinElevation
                ).mag(mag);
            }
            return xyz;
        }

        public float x() {
            return xyz().x();
        }

        public float y() {
            return xyz().y();
        }

        public float z() {
            return xyz().z();
        }

        public float mag() {
            return mag;
        }

        public float magSquared() {
            return mag*mag;
        }

        public Vec3 mag(float newMag) {
            return azimuthAndElevation(azimuth, elevation, newMag);
        }

        public Vec3 mult(float factor) {
            return azimuthAndElevation(azimuth, elevation, mag * factor);
        }

        public Vec3 div(float divisor) {
            return azimuthAndElevation(azimuth, elevation, mag / divisor);
        }

        public Vec3 rot90xy() {
            return xyz().rot90xy();
        }

        public Vec2 xy() {
            return angleVec2(azimuth, mag);
        }
    }

    /**
     * Start with (1, 0, 0).
     * Rotate by the azimuth angle on the XY plane.
     * Rotate toward the Z axis by the elevation angle.
     */
    public static Vec3 azimuthAndElevation(float azimuth, float elevation, float mag) {
        if (abs(mag) < EPSILON) return origin3();
        return new AzimuthAndElevation(azimuth, elevation, mag);
    }

    private static class PointAndDirection3 extends BaseLine3 {

        final Vec3 a;
        final Vec3 ab;

        private PointAndDirection3(Vec3 a, Vec3 ab) {
            this.a = a;
            this.ab = ab;
        }

        public Vec3 a() {
            return a;
        }

        public Vec3 b() {
            return a.add(ab);
        }

        public Vec3 ab() {
            return ab;
        }

        public Line3 add(Vec3 offset) {
            return new PointAndDirection3(a.add(offset), ab);
        }

        public Line3 sub(Vec3 offset) {
            return new PointAndDirection3(a.sub(offset), ab);
        }

        public Line3 reverse() {
            return new PointAndDirection3(b(), ab.mult(-1));
        }

        public Vec3 midpoint() {
            return a.add(ab.div(2));
        }

    }

    public static Line3 pointAndStep(Vec3 a, Vec3 ab) {
        return new PointAndDirection3(a, ab);
    }

    public static Line3 pointAndStep(IsVec3 a, IsVec3 ab) {
        return new PointAndDirection3(a.asVec3(), ab.asVec3());
    }

    public static Line3 oTo3(Vec3 b) {
        return aToB(origin3(), b);
    }

    public static float distance(Line3 line, IsVec3 c) {
        return distance(line, c.asVec3());
    }

    public static float distance(Line3 line, Vec3 c) {
        // http://mathworld.wolfram.com/Point-LineDistance3-Dimensional.html
        Vec3 a = line.a(), b = line.b(), ac = aToB(a, c).ab(), bc = aToB(b, c).ab(), ab = line.ab();
        return ac.cross(bc).mag() / ab.mag();
    }

    private static Vec3 matrixApply(float[][] m, Vec3 x) {
        return xyz(
            m[0][0] * x.x() + m[0][1] * x.y() + m[0][2] * x.z(),
            m[1][0] * x.x() + m[1][1] * x.y() + m[1][2] * x.z(),
            m[2][0] * x.x() + m[2][1] * x.y() + m[2][2] * x.z()
        );
    }

    private static float[][] rotationMatrix(Vec3 axis, float angle) {
        // http://en.wikipedia.org/wiki/Rotation_matrix
        Vec3 u = axis.unit();
        float ux = u.x(), uy = u.y(), uz = u.z();
        float cosa = (float) cos(angle), sina = (float) sin(angle);
        return new float[][]{
            { cosa + ux*ux*(1-cosa), ux*uy*(1-cosa)-uz*sina, ux*uz*(1-cosa)+uy*sina },
            { uy*ux*(1-cosa)+uz*sina, cosa+uy*uy*(1-cosa), uy*uz*(1-cosa)-ux*sina },
            { uz*ux*(1-cosa)-uy*sina, uz*uy*(1-cosa)+ux*sina, cosa+uz*uz*(1-cosa) }
        };
    }

    public static Vec3 rotatePointAroundLine(Line3 line, IsVec3 c, float angle) {
        return rotatePointAroundLine(line, c.asVec3(), angle);
    }

    public static Vec3 rotatePointAroundLine(Line3 line, Vec3 c, float angle) {

        float[][] matrix = rotationMatrix(line.ab(), angle);

        c = c.sub(line.a());
        c = matrixApply(matrix, c);
        c = c.add(line.a());

        return c;
    }

    public interface Sphere {

        Vec3 center();

        float radius();

    }

    private static class SimpleSphere implements Sphere {

        final Vec3 center;
        final float radius;

        private SimpleSphere(Vec3 center, float radius) {
            this.center = center;
            this.radius = radius;
        }

        public Vec3 center() {
            return center;
        }

        public float radius() {
            return radius;
        }
    }

    public static Sphere sphere(Vec3 center, float radius) {
        return new SimpleSphere(center, radius);
    }

    public static Sphere sphere(IsVec3 center, float radius) {
        return sphere(center.asVec3(), radius);
    }

    public interface Circle3 {

        Vec3 center();

        /** A vector orthogonal to the plane on which the circle lies. */
        Vec3 normal();

        float radius();

        /** This sphere's intersection with the XY plane. 0 or 1 circles. */
        List<Circle2> intersectXY();

        /** This sphere's intersection with the origin-intersectingplane normal to the vector. 0 or 1 circles. */
        List<Circle3> intersectPlane(Vec3 normal);

    }

    public static class SimpleCircle3 implements Circle3 {

        final Vec3 center;
        final Vec3 normal;
        final float radius;

        public SimpleCircle3(Vec3 center, Vec3 normal, float radius) {
            this.center = center;
            this.normal = normal;
            this.radius = radius;
        }

        public Vec3 center() {
            return center;
        }

        public Vec3 normal() {
            return normal;
        }

        public float radius() {
            return radius;
        }

        public List<Circle2> intersectXY() {
            // http://en.wikipedia.org/wiki/Plane%E2%80%93sphere_intersection
            float r = radius, d = center.z();
            if (d > r) {
                return asList(new Circle2[] {});
            }
            return asList(circle(center.xy(), r*r - d*d));
        }

        public List<Circle3> intersectPlane(Vec3 normal) {
            return null; // todo
        }
    }

    public static Circle3 circle(Vec3 center, Vec3 normal, float radius) {
        return new SimpleCircle3(center, normal, radius);
    }

    /** 0 or 1 circles. */
    public static List<Circle3> intersect(Sphere a, Sphere b) {
        // http://mathworld.wolfram.com/Sphere-SphereIntersection.html
        float R = a.radius(), r = b.radius();
        if (distance(a.center(), b.center()) > R + r) {
            return asList(new Circle3[]{});
        }
        Line3 ab = aToB(a.center(), b.center());
        float d = ab.mag();
        float x = (d*d - r*r + R*R) / (2 * d);
        Vec3 center = ab.a().add(ab.ab().mag(x));
        float rad = (float) sqrt((r-d-R) * (R-d-r) * (r-d+R) * (d+r+R)) / (2 * d);
        return asList(circle(center, ab.ab(), rad));
    }

    public interface IsVec3 {

        Vec3 asVec3();

    }

    public static Vec3 parseXYZ(String s) {
        String[] ss = s.split(",");
        return xyz(
            Float.parseFloat(ss[0]),
            Float.parseFloat(ss[1]),
            Float.parseFloat(ss[2])
        );
    }

    public static String formatXYZ(Vec3 v) {
        return String.format("%f,%f,%f", v.x(), v.y(), v.z());
    }

}

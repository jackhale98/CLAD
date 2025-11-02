/*
 * occt-wrapper.h - C wrapper for OpenCASCADE Technology (OCCT)
 *
 * This header defines a C API for OCCT operations. All C++ exceptions
 * are caught at the boundary and converted to error codes.
 */

#ifndef OCCT_WRAPPER_H
#define OCCT_WRAPPER_H

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Error Codes
 */
#define OCCT_SUCCESS 0
#define OCCT_ERROR_UNKNOWN -1
#define OCCT_ERROR_DOMAIN -2
#define OCCT_ERROR_CONSTRUCTION -3
#define OCCT_ERROR_NULL_OBJECT -4

/*
 * Opaque handle to OCCT shape
 * In C++, this is TopoDS_Shape*
 */
typedef void* occt_shape_t;

/*
 * Primitive Constructors
 */

/**
 * Create a box primitive
 * @param width Box width (X dimension)
 * @param height Box height (Y dimension)
 * @param depth Box depth (Z dimension)
 * @param out_shape Pointer to receive the created shape
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_box(double width, double height, double depth,
                  occt_shape_t* out_shape, char** out_error);

/**
 * Create a cylinder primitive
 * @param radius Cylinder radius
 * @param height Cylinder height
 * @param out_shape Pointer to receive the created shape
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_cylinder(double radius, double height,
                       occt_shape_t* out_shape, char** out_error);

/**
 * Create a sphere primitive
 * @param radius Sphere radius
 * @param out_shape Pointer to receive the created shape
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_sphere(double radius,
                     occt_shape_t* out_shape, char** out_error);

/**
 * Create a cone/truncated cone primitive
 * @param radius1 Bottom radius
 * @param radius2 Top radius
 * @param height Cone height
 * @param out_shape Pointer to receive the created shape
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_cone(double radius1, double radius2, double height,
                   occt_shape_t* out_shape, char** out_error);

/*
 * Boolean Operations
 */

/**
 * Union (fuse) two shapes
 * @param shape1 First shape
 * @param shape2 Second shape
 * @param out_shape Pointer to receive the result
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_union(occt_shape_t shape1, occt_shape_t shape2,
               occt_shape_t* out_shape, char** out_error);

/**
 * Cut (subtract) shape2 from shape1
 * @param shape1 Base shape
 * @param shape2 Tool shape to subtract
 * @param out_shape Pointer to receive the result
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_cut(occt_shape_t shape1, occt_shape_t shape2,
             occt_shape_t* out_shape, char** out_error);

/**
 * Intersect two shapes
 * @param shape1 First shape
 * @param shape2 Second shape
 * @param out_shape Pointer to receive the result
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_intersect(occt_shape_t shape1, occt_shape_t shape2,
                   occt_shape_t* out_shape, char** out_error);

/*
 * Transformations
 */

/**
 * Translate a shape
 * @param shape Input shape
 * @param dx Translation in X
 * @param dy Translation in Y
 * @param dz Translation in Z
 * @param out_shape Pointer to receive the result
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_translate(occt_shape_t shape, double dx, double dy, double dz,
                   occt_shape_t* out_shape, char** out_error);

/**
 * Rotate a shape around an axis
 * @param shape Input shape
 * @param axis_x X component of rotation axis
 * @param axis_y Y component of rotation axis
 * @param axis_z Z component of rotation axis
 * @param angle Rotation angle in degrees
 * @param out_shape Pointer to receive the result
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_rotate(occt_shape_t shape,
                double axis_x, double axis_y, double axis_z,
                double angle,
                occt_shape_t* out_shape, char** out_error);

/**
 * Mirror a shape across a plane
 * @param shape Input shape
 * @param plane_x X component of plane normal
 * @param plane_y Y component of plane normal
 * @param plane_z Z component of plane normal
 * @param out_shape Pointer to receive the result
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_mirror(occt_shape_t shape,
                double plane_x, double plane_y, double plane_z,
                occt_shape_t* out_shape, char** out_error);

/**
 * Scale a shape uniformly
 * @param shape Input shape
 * @param scale_factor Scale factor (must be positive, non-zero)
 * @param out_shape Pointer to receive the result
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_scale(occt_shape_t shape, double scale_factor,
               occt_shape_t* out_shape, char** out_error);

/*
 * Export Operations
 */

/**
 * Export shape to STEP file
 * @param shape Shape to export
 * @param filename Output filename
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_export_step(occt_shape_t shape, const char* filename,
                     char** out_error);

/**
 * Export shape to STL file
 * @param shape Shape to export
 * @param filename Output filename
 * @param linear_deflection Linear deflection for tessellation
 * @param angular_deflection Angular deflection for tessellation
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_export_stl(occt_shape_t shape, const char* filename,
                    double linear_deflection, double angular_deflection,
                    char** out_error);

/**
 * Export shape to glTF file (binary .glb format)
 * @param shape Shape to export
 * @param filename Output filename (.glb or .gltf)
 * @param linear_deflection Linear deflection for tessellation (0.1 is good default)
 * @param angular_deflection Angular deflection for tessellation in radians (0.5 is good default)
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_export_gltf(occt_shape_t shape, const char* filename,
                     double linear_deflection, double angular_deflection,
                     char** out_error);

/*
 * Shape Queries
 */

/**
 * Get bounding box of a shape
 * @param shape Input shape
 * @param xmin Pointer to receive minimum X coordinate
 * @param ymin Pointer to receive minimum Y coordinate
 * @param zmin Pointer to receive minimum Z coordinate
 * @param xmax Pointer to receive maximum X coordinate
 * @param ymax Pointer to receive maximum Y coordinate
 * @param zmax Pointer to receive maximum Z coordinate
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_get_bounding_box(occt_shape_t shape,
                          double* xmin, double* ymin, double* zmin,
                          double* xmax, double* ymax, double* zmax,
                          char** out_error);

/**
 * Get volume of a solid shape
 * @param shape Input shape (must be a solid)
 * @param volume Pointer to receive the volume
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_get_volume(occt_shape_t shape, double* volume, char** out_error);

/**
 * Get area of a face or shell
 * @param shape Input shape (must be a face or shell)
 * @param area Pointer to receive the area
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_get_area(occt_shape_t shape, double* area, char** out_error);

/**
 * Get length of an edge or wire
 * @param shape Input shape (must be an edge or wire)
 * @param length Pointer to receive the length
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_get_length(occt_shape_t shape, double* length, char** out_error);

/**
 * Get center of mass of a shape
 * @param shape Input shape
 * @param x Pointer to receive X coordinate
 * @param y Pointer to receive Y coordinate
 * @param z Pointer to receive Z coordinate
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_get_center_of_mass(occt_shape_t shape,
                             double* x, double* y, double* z,
                             char** out_error);

/**
 * Count sub-shapes of a given type
 * @param shape Input shape
 * @param shape_type Type to count (0=vertex, 1=edge, 2=wire, 3=face, 4=shell, 5=solid, 6=compound)
 * @param count Pointer to receive the count
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_count_shapes(occt_shape_t shape, int shape_type, int* count, char** out_error);

/**
 * Get sub-shapes of a given type
 * @param shape Input shape
 * @param shape_type Type to get (0=vertex, 1=edge, 2=wire, 3=face, 4=shell, 5=solid, 6=compound)
 * @param out_shapes Array to receive the shapes (must be pre-allocated)
 * @param max_shapes Maximum number of shapes to return
 * @param out_count Pointer to receive actual count
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_get_shapes(occt_shape_t shape, int shape_type,
                    occt_shape_t* out_shapes, int max_shapes, int* out_count,
                    char** out_error);

/**
 * Get normal vector of a face at its center point
 * @param face Input shape (must be a face)
 * @param nx Pointer to receive X component of normal
 * @param ny Pointer to receive Y component of normal
 * @param nz Pointer to receive Z component of normal
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_get_face_normal(occt_shape_t face,
                         double* nx, double* ny, double* nz,
                         char** out_error);

/**
 * Get center point of a face
 * @param face Input shape (must be a face)
 * @param x Pointer to receive X coordinate of center
 * @param y Pointer to receive Y coordinate of center
 * @param z Pointer to receive Z coordinate of center
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_get_face_center(occt_shape_t face,
                         double* x, double* y, double* z,
                         char** out_error);

/**
 * Get geometric type of an edge
 * @param edge Input shape (must be an edge)
 * @param geom_type Pointer to receive the geometry type
 *        0=line, 1=circle, 2=ellipse, 3=hyperbola, 4=parabola,
 *        5=bezier, 6=bspline, 7=other
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_get_edge_geom_type(occt_shape_t edge, int* geom_type,
                            char** out_error);

/**
 * Get geometric type of a face
 * @param face Input shape (must be a face)
 * @param geom_type Pointer to receive the geometry type
 *        0=plane, 1=cylinder, 2=cone, 3=sphere, 4=torus,
 *        5=bezier, 6=bspline, 7=other
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_get_face_geom_type(occt_shape_t face, int* geom_type,
                            char** out_error);

/*
 * Advanced Features - Fillets
 */

/**
 * Create fillet on edges of a shape
 * @param shape Input shape (solid) to fillet
 * @param edges Array of edge shapes to fillet
 * @param num_edges Number of edges in array
 * @param radius Fillet radius (must be positive)
 * @param out_shape Pointer to receive the filleted shape
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_fillet(occt_shape_t shape,
                     occt_shape_t* edges,
                     int num_edges,
                     double radius,
                     occt_shape_t* out_shape,
                     char** out_error);

/**
 * Create chamfer on edges of a shape
 * @param shape Input shape (solid) to chamfer
 * @param edges Array of edge shapes to chamfer
 * @param num_edges Number of edges in array
 * @param distance Chamfer distance (must be positive)
 * @param out_shape Pointer to receive the chamfered shape
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_chamfer(occt_shape_t shape,
                      occt_shape_t* edges,
                      int num_edges,
                      double distance,
                      occt_shape_t* out_shape,
                      char** out_error);

/*
 * Advanced Features - Curves and Splines
 */

/**
 * Create interpolated B-spline curve through points
 * @param points Array of points (x,y,z triplets)
 * @param num_points Number of points (must be >= 2)
 * @param closed 1 for closed curve, 0 for open
 * @param out_edge Pointer to receive the edge
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_interpolated_curve(const double* points, int num_points,
                                   int closed,
                                   occt_shape_t* out_edge,
                                   char** out_error);

/**
 * Create Bezier curve from control points
 * @param control_points Array of control points (x,y,z triplets)
 * @param num_points Number of control points (2-25)
 * @param out_edge Pointer to receive the edge
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_bezier_curve(const double* control_points, int num_points,
                            occt_shape_t* out_edge,
                            char** out_error);

/**
 * Create circular arc through 3 points
 * @param x1,y1,z1 First point
 * @param x2,y2,z2 Second point
 * @param x3,y3,z3 Third point
 * @param out_edge Pointer to receive the edge
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_arc_3points(double x1, double y1, double z1,
                           double x2, double y2, double z2,
                           double x3, double y3, double z3,
                           occt_shape_t* out_edge,
                           char** out_error);

/**
 * Create circular arc by center, radius, and angles
 * @param cx,cy,cz Center point
 * @param radius Arc radius (must be positive)
 * @param start_angle Start angle in degrees
 * @param end_angle End angle in degrees
 * @param axis_x,axis_y,axis_z Axis of rotation
 * @param out_edge Pointer to receive the edge
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_arc_center_radius(double cx, double cy, double cz,
                                 double radius,
                                 double start_angle, double end_angle,
                                 double axis_x, double axis_y, double axis_z,
                                 occt_shape_t* out_edge,
                                 char** out_error);

/**
 * Create wire from connected edges
 * @param edges Array of edge shapes
 * @param num_edges Number of edges
 * @param out_wire Pointer to receive the wire
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_wire(occt_shape_t* edges, int num_edges,
                   occt_shape_t* out_wire,
                   char** out_error);

/**
 * Check if wire is closed
 * @param wire Input wire shape
 * @param is_closed Pointer to receive result (1=closed, 0=open)
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_wire_is_closed(occt_shape_t wire, int* is_closed, char** out_error);

/*
 * Advanced Features - Sweeps
 */

/**
 * Sweep a profile along a path (pipe operation)
 * @param profile Profile shape (wire or face) to sweep
 * @param path Path to follow (wire)
 * @param out_shape Pointer to receive the swept solid or shell
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_pipe(occt_shape_t profile, occt_shape_t path,
                   occt_shape_t* out_shape,
                   char** out_error);

/**
 * Sweep a circular profile along a path (pipe with radius)
 * @param path Path to follow (wire)
 * @param radius Circular profile radius (must be positive)
 * @param out_shape Pointer to receive the swept solid
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_pipe_shell(occt_shape_t path, double radius,
                         occt_shape_t* out_shape,
                         char** out_error);

/*
 * Advanced Features - Lofts
 */

/**
 * Create loft through multiple sections
 * @param sections Array of section shapes (wires or faces)
 * @param num_sections Number of sections (must be >= 2)
 * @param solid 1 for solid, 0 for shell
 * @param ruled 1 for ruled surface, 0 for smooth
 * @param out_shape Pointer to receive the lofted shape
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_loft(occt_shape_t* sections, int num_sections,
                   int solid, int ruled,
                   occt_shape_t* out_shape,
                   char** out_error);

/*
 * Advanced Features - Shelling
 */

/**
 * Create hollow shell from solid
 * @param solid Input solid shape
 * @param faces_to_remove Array of face handles to remove (can be NULL for no removal)
 * @param num_faces Number of faces to remove
 * @param thickness Wall thickness (negative for inward, positive for outward)
 * @param tolerance Tolerance for offset operation
 * @param out_shape Pointer to receive the shelled shape
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_make_shell(occt_shape_t solid,
                    occt_shape_t* faces_to_remove,
                    int num_faces,
                    double thickness,
                    double tolerance,
                    occt_shape_t* out_shape,
                    char** out_error);

/*
 * Advanced Features - Mirroring
 */

/**
 * Mirror shape across a plane
 * @param shape Input shape to mirror
 * @param plane_ox,plane_oy,plane_oz Origin point of mirror plane
 * @param plane_nx,plane_ny,plane_nz Normal vector of mirror plane
 * @param out_shape Pointer to receive the mirrored shape
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_mirror_shape(occt_shape_t shape,
                      double plane_ox, double plane_oy, double plane_oz,
                      double plane_nx, double plane_ny, double plane_nz,
                      occt_shape_t* out_shape,
                      char** out_error);

/*
 * Memory Management
 */

/**
 * Release (delete) a shape
 * @param shape Shape to release
 */
void occt_release_shape(occt_shape_t shape);

/**
 * Copy a shape
 * @param shape Shape to copy
 * @param out_shape Pointer to receive the copy
 * @param out_error Pointer to receive error message (if any)
 * @return Error code (OCCT_SUCCESS on success)
 */
int occt_copy_shape(occt_shape_t shape,
                    occt_shape_t* out_shape, char** out_error);

/**
 * Check if a shape is null
 * @param shape Shape to check
 * @return 1 if null, 0 otherwise
 */
int occt_shape_is_null(occt_shape_t shape);

#ifdef __cplusplus
}
#endif

#endif // OCCT_WRAPPER_H

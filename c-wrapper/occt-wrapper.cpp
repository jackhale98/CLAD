/*
 * occt-wrapper.cpp - C wrapper implementation for OpenCASCADE Technology
 *
 * This file implements the C API wrapper, catching all C++ exceptions
 * and converting them to error codes.
 */

#include "occt-wrapper.h"

#include <cstring>
#include <cstdlib>
#include <string>

// OCCT includes
#include <TopoDS_Shape.hxx>
#include <TopoDS.hxx>
#include <TopoDS_Face.hxx>
#include <BRepPrimAPI_MakeBox.hxx>
#include <BRepPrimAPI_MakeCylinder.hxx>
#include <BRepPrimAPI_MakeSphere.hxx>
#include <BRepPrimAPI_MakeCone.hxx>
#include <BRepAlgoAPI_Fuse.hxx>
#include <BRepAlgoAPI_Cut.hxx>
#include <BRepAlgoAPI_Common.hxx>
#include <BRepBuilderAPI_Transform.hxx>
#include <gp_Trsf.hxx>
#include <gp_Vec.hxx>
#include <gp_Ax1.hxx>
#include <gp_Pnt.hxx>
#include <gp_Dir.hxx>
#include <Standard_Failure.hxx>
#include <Standard_DomainError.hxx>
#include <Standard_ConstructionError.hxx>
#include <Standard_NullObject.hxx>
#include <STEPControl_Writer.hxx>
#include <StlAPI_Writer.hxx>
#include <BRepMesh_IncrementalMesh.hxx>
#include <RWGltf_CafWriter.hxx>
#include <TDocStd_Document.hxx>
#include <XCAFDoc_DocumentTool.hxx>
#include <XCAFApp_Application.hxx>
#include <XCAFDoc_ShapeTool.hxx>
#include <TDataStd_Name.hxx>
#include <TDF_Label.hxx>
#include <Bnd_Box.hxx>
#include <BRepBndLib.hxx>
#include <GProp_GProps.hxx>
#include <BRepGProp.hxx>
#include <TopExp_Explorer.hxx>
#include <TopAbs.hxx>
#include <BRepAdaptor_Surface.hxx>
#include <BRepAdaptor_Curve.hxx>
#include <BRepTools.hxx>
#include <GeomLProp_SLProps.hxx>
#include <GeomAbs_CurveType.hxx>
#include <GeomAbs_SurfaceType.hxx>
#include <BRepFilletAPI_MakeFillet.hxx>
#include <BRepFilletAPI_MakeChamfer.hxx>
#include <TopoDS_Edge.hxx>
#include <TopoDS_Wire.hxx>
#include <TColgp_Array1OfPnt.hxx>
#include <GeomAPI_PointsToBSpline.hxx>
#include <Geom_BSplineCurve.hxx>
#include <Geom_BezierCurve.hxx>
#include <Geom_TrimmedCurve.hxx>
#include <BRepBuilderAPI_MakeEdge.hxx>
#include <BRepBuilderAPI_MakeWire.hxx>
#include <GC_MakeArcOfCircle.hxx>
#include <gp_Circ.hxx>
#include <BRep_Tool.hxx>
#include <BRepOffsetAPI_MakePipe.hxx>
#include <BRepOffsetAPI_MakePipeShell.hxx>
#include <BRepOffsetAPI_ThruSections.hxx>
#include <BRepOffsetAPI_MakeThickSolid.hxx>
#include <BRepPrimAPI_MakePrism.hxx>
#include <TopTools_ListOfShape.hxx>
#include <gp_Ax2.hxx>

/*
 * Helper macros for exception handling
 */
#define TRY_CATCH_BEGIN try {
#define TRY_CATCH_END(out_error) \
    } catch (const Standard_DomainError& e) { \
        if (out_error) *out_error = strdup(e.GetMessageString()); \
        return OCCT_ERROR_DOMAIN; \
    } catch (const Standard_ConstructionError& e) { \
        if (out_error) *out_error = strdup(e.GetMessageString()); \
        return OCCT_ERROR_CONSTRUCTION; \
    } catch (const Standard_NullObject& e) { \
        if (out_error) *out_error = strdup(e.GetMessageString()); \
        return OCCT_ERROR_NULL_OBJECT; \
    } catch (const Standard_Failure& e) { \
        if (out_error) *out_error = strdup(e.GetMessageString()); \
        return OCCT_ERROR_UNKNOWN; \
    } catch (const std::exception& e) { \
        if (out_error) *out_error = strdup(e.what()); \
        return OCCT_ERROR_UNKNOWN; \
    } catch (...) { \
        if (out_error) *out_error = strdup("Unknown C++ exception"); \
        return OCCT_ERROR_UNKNOWN; \
    }

/*
 * Primitive Constructors
 */

int occt_make_box(double width, double height, double depth,
                  occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        BRepPrimAPI_MakeBox maker(width, height, depth);
        TopoDS_Shape* shape = new TopoDS_Shape(maker.Shape());
        *out_shape = shape;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_make_cylinder(double radius, double height,
                       occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        BRepPrimAPI_MakeCylinder maker(radius, height);
        TopoDS_Shape* shape = new TopoDS_Shape(maker.Shape());
        *out_shape = shape;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_make_sphere(double radius,
                     occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        BRepPrimAPI_MakeSphere maker(radius);
        TopoDS_Shape* shape = new TopoDS_Shape(maker.Shape());
        *out_shape = shape;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_make_cone(double radius1, double radius2, double height,
                   occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        BRepPrimAPI_MakeCone maker(radius1, radius2, height);
        TopoDS_Shape* shape = new TopoDS_Shape(maker.Shape());
        *out_shape = shape;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

/*
 * Boolean Operations
 */

int occt_union(occt_shape_t shape1, occt_shape_t shape2,
               occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!shape1 || !shape2) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s1 = static_cast<TopoDS_Shape*>(shape1);
        TopoDS_Shape* s2 = static_cast<TopoDS_Shape*>(shape2);

        BRepAlgoAPI_Fuse fuseMaker(*s1, *s2);
        fuseMaker.Build();

        if (!fuseMaker.IsDone()) {
            if (out_error) *out_error = strdup("Boolean union operation failed");
            return OCCT_ERROR_CONSTRUCTION;
        }

        TopoDS_Shape* result = new TopoDS_Shape(fuseMaker.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_cut(occt_shape_t shape1, occt_shape_t shape2,
             occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!shape1 || !shape2) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s1 = static_cast<TopoDS_Shape*>(shape1);
        TopoDS_Shape* s2 = static_cast<TopoDS_Shape*>(shape2);

        BRepAlgoAPI_Cut cutMaker(*s1, *s2);
        cutMaker.Build();

        if (!cutMaker.IsDone()) {
            if (out_error) *out_error = strdup("Boolean cut operation failed");
            return OCCT_ERROR_CONSTRUCTION;
        }

        TopoDS_Shape* result = new TopoDS_Shape(cutMaker.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_intersect(occt_shape_t shape1, occt_shape_t shape2,
                   occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!shape1 || !shape2) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s1 = static_cast<TopoDS_Shape*>(shape1);
        TopoDS_Shape* s2 = static_cast<TopoDS_Shape*>(shape2);

        BRepAlgoAPI_Common commonMaker(*s1, *s2);
        commonMaker.Build();

        if (!commonMaker.IsDone()) {
            if (out_error) *out_error = strdup("Boolean intersect operation failed");
            return OCCT_ERROR_CONSTRUCTION;
        }

        TopoDS_Shape* result = new TopoDS_Shape(commonMaker.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

/*
 * Transformations
 */

int occt_translate(occt_shape_t shape, double dx, double dy, double dz,
                   occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        gp_Trsf transformation;
        transformation.SetTranslation(gp_Vec(dx, dy, dz));

        BRepBuilderAPI_Transform transformer(*s, transformation, Standard_True);
        TopoDS_Shape* result = new TopoDS_Shape(transformer.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_rotate(occt_shape_t shape,
                double axis_x, double axis_y, double axis_z,
                double angle,
                occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        // Convert angle from degrees to radians
        double angle_rad = angle * M_PI / 180.0;

        // Create rotation axis through origin
        gp_Ax1 axis(gp_Pnt(0, 0, 0), gp_Dir(axis_x, axis_y, axis_z));

        gp_Trsf transformation;
        transformation.SetRotation(axis, angle_rad);

        BRepBuilderAPI_Transform transformer(*s, transformation, Standard_True);
        TopoDS_Shape* result = new TopoDS_Shape(transformer.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_mirror(occt_shape_t shape,
                double plane_x, double plane_y, double plane_z,
                occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        // Create mirror plane through origin with given normal
        gp_Ax2 plane(gp_Pnt(0, 0, 0), gp_Dir(plane_x, plane_y, plane_z));

        gp_Trsf transformation;
        transformation.SetMirror(plane);

        BRepBuilderAPI_Transform transformer(*s, transformation, Standard_True);
        TopoDS_Shape* result = new TopoDS_Shape(transformer.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_scale(occt_shape_t shape, double scale_factor,
               occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (scale_factor <= 0.0) {
        if (out_error) *out_error = strdup("Scale factor must be positive");
        return OCCT_ERROR_DOMAIN;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        gp_Trsf transformation;
        transformation.SetScale(gp_Pnt(0, 0, 0), scale_factor);

        BRepBuilderAPI_Transform transformer(*s, transformation, Standard_True);
        TopoDS_Shape* result = new TopoDS_Shape(transformer.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

/*
 * Export Operations
 */

int occt_export_step(occt_shape_t shape, const char* filename,
                     char** out_error)
{
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!filename) {
        if (out_error) *out_error = strdup("Null filename");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        STEPControl_Writer writer;
        IFSelect_ReturnStatus status = writer.Transfer(*s, STEPControl_AsIs);

        if (status != IFSelect_RetDone) {
            if (out_error) *out_error = strdup("Failed to transfer shape to STEP");
            return OCCT_ERROR_CONSTRUCTION;
        }

        status = writer.Write(filename);

        if (status != IFSelect_RetDone) {
            if (out_error) *out_error = strdup("Failed to write STEP file");
            return OCCT_ERROR_CONSTRUCTION;
        }

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_export_stl(occt_shape_t shape, const char* filename,
                    double linear_deflection, double angular_deflection,
                    char** out_error)
{
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!filename) {
        if (out_error) *out_error = strdup("Null filename");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        // Tessellate the shape first
        BRepMesh_IncrementalMesh mesher(*s, linear_deflection, Standard_False, angular_deflection);

        if (!mesher.IsDone()) {
            if (out_error) *out_error = strdup("Failed to tessellate shape");
            return OCCT_ERROR_CONSTRUCTION;
        }

        // Write STL file
        StlAPI_Writer writer;
        Standard_Boolean result = writer.Write(*s, filename);

        if (!result) {
            if (out_error) *out_error = strdup("Failed to write STL file");
            return OCCT_ERROR_CONSTRUCTION;
        }

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_export_gltf(occt_shape_t shape, const char* filename,
                     double linear_deflection, double angular_deflection,
                     char** out_error)
{
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!filename) {
        if (out_error) *out_error = strdup("Null filename");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        // Tessellate the shape first
        BRepMesh_IncrementalMesh mesher(*s, linear_deflection, Standard_False, angular_deflection);

        if (!mesher.IsDone()) {
            if (out_error) *out_error = strdup("Failed to tessellate shape");
            return OCCT_ERROR_CONSTRUCTION;
        }

        // Create XCAF document to hold the shape
        Handle(XCAFApp_Application) app = XCAFApp_Application::GetApplication();
        Handle(TDocStd_Document) doc;
        app->NewDocument("BinXCAF", doc);

        // Get shape tool and add the shape
        Handle(XCAFDoc_ShapeTool) shapeTool = XCAFDoc_DocumentTool::ShapeTool(doc->Main());
        TDF_Label shapeLabel = shapeTool->AddShape(*s, Standard_False);

        // Set a name for the shape
        TDataStd_Name::Set(shapeLabel, "Shape");

        // Create glTF writer and export
        RWGltf_CafWriter writer(filename, Standard_True);  // Standard_True for binary format (.glb)
        writer.SetParallel(Standard_True);  // Use parallel processing if available

        // Perform the export
        if (!writer.Perform(doc, TColStd_IndexedDataMapOfStringString(), Message_ProgressRange())) {
            if (out_error) *out_error = strdup("Failed to write glTF file");
            return OCCT_ERROR_CONSTRUCTION;
        }

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

/*
 * Shape Queries
 */

int occt_get_bounding_box(occt_shape_t shape,
                          double* xmin, double* ymin, double* zmin,
                          double* xmax, double* ymax, double* zmax,
                          char** out_error)
{
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!xmin || !ymin || !zmin || !xmax || !ymax || !zmax) {
        if (out_error) *out_error = strdup("Null output parameters");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        Bnd_Box bbox;
        BRepBndLib::Add(*s, bbox);

        bbox.Get(*xmin, *ymin, *zmin, *xmax, *ymax, *zmax);

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_get_volume(occt_shape_t shape, double* volume, char** out_error)
{
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!volume) {
        if (out_error) *out_error = strdup("Null output parameter");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        GProp_GProps props;
        BRepGProp::VolumeProperties(*s, props);

        *volume = props.Mass();

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_get_area(occt_shape_t shape, double* area, char** out_error)
{
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!area) {
        if (out_error) *out_error = strdup("Null output parameter");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        GProp_GProps props;
        BRepGProp::SurfaceProperties(*s, props);

        *area = props.Mass();

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_get_length(occt_shape_t shape, double* length, char** out_error)
{
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!length) {
        if (out_error) *out_error = strdup("Null output parameter");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        GProp_GProps props;
        BRepGProp::LinearProperties(*s, props);

        *length = props.Mass();

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_get_center_of_mass(occt_shape_t shape,
                             double* x, double* y, double* z,
                             char** out_error)
{
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!x || !y || !z) {
        if (out_error) *out_error = strdup("Null output parameters");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        GProp_GProps props;
        BRepGProp::VolumeProperties(*s, props);

        gp_Pnt center = props.CentreOfMass();
        *x = center.X();
        *y = center.Y();
        *z = center.Z();

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_count_shapes(occt_shape_t shape, int shape_type, int* count, char** out_error)
{
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!count) {
        if (out_error) *out_error = strdup("Null count parameter");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        // Map shape_type integer to TopAbs_ShapeEnum
        TopAbs_ShapeEnum type;
        switch (shape_type) {
            case 0: type = TopAbs_VERTEX; break;
            case 1: type = TopAbs_EDGE; break;
            case 2: type = TopAbs_WIRE; break;
            case 3: type = TopAbs_FACE; break;
            case 4: type = TopAbs_SHELL; break;
            case 5: type = TopAbs_SOLID; break;
            case 6: type = TopAbs_COMPOUND; break;
            default:
                if (out_error) *out_error = strdup("Invalid shape type");
                return OCCT_ERROR_DOMAIN;
        }

        // Count shapes using TopExp_Explorer
        int cnt = 0;
        for (TopExp_Explorer exp(*s, type); exp.More(); exp.Next()) {
            cnt++;
        }

        *count = cnt;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_get_shapes(occt_shape_t shape, int shape_type,
                    occt_shape_t* out_shapes, int max_shapes, int* out_count,
                    char** out_error)
{
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!out_shapes || !out_count) {
        if (out_error) *out_error = strdup("Null output parameters");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        // Map shape_type integer to TopAbs_ShapeEnum
        TopAbs_ShapeEnum type;
        switch (shape_type) {
            case 0: type = TopAbs_VERTEX; break;
            case 1: type = TopAbs_EDGE; break;
            case 2: type = TopAbs_WIRE; break;
            case 3: type = TopAbs_FACE; break;
            case 4: type = TopAbs_SHELL; break;
            case 5: type = TopAbs_SOLID; break;
            case 6: type = TopAbs_COMPOUND; break;
            default:
                if (out_error) *out_error = strdup("Invalid shape type");
                return OCCT_ERROR_DOMAIN;
        }

        // Collect shapes using TopExp_Explorer
        int cnt = 0;
        for (TopExp_Explorer exp(*s, type); exp.More() && cnt < max_shapes; exp.Next()) {
            TopoDS_Shape* sub_shape = new TopoDS_Shape(exp.Current());
            out_shapes[cnt++] = sub_shape;
        }

        *out_count = cnt;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_get_face_normal(occt_shape_t face,
                         double* nx, double* ny, double* nz,
                         char** out_error)
{
    if (!face) {
        if (out_error) *out_error = strdup("Null input face");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!nx || !ny || !nz) {
        if (out_error) *out_error = strdup("Null output parameters");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(face);

        // Check that it's actually a face
        if (s->ShapeType() != TopAbs_FACE) {
            if (out_error) *out_error = strdup("Shape is not a face");
            return OCCT_ERROR_DOMAIN;
        }

        TopoDS_Face f = TopoDS::Face(*s);

        // Get UV bounds
        double umin, umax, vmin, vmax;
        BRepTools::UVBounds(f, umin, umax, vmin, vmax);

        // Evaluate at center of parameter space
        double u = (umin + umax) / 2.0;
        double v = (vmin + vmax) / 2.0;

        // Create surface adaptor
        BRepAdaptor_Surface surface(f);

        // Get normal using surface properties
        GeomLProp_SLProps props(surface.Surface().Surface(), u, v, 1, 1e-9);

        if (!props.IsNormalDefined()) {
            if (out_error) *out_error = strdup("Normal is not defined at center point");
            return OCCT_ERROR_CONSTRUCTION;
        }

        gp_Dir normal = props.Normal();

        // Adjust for face orientation
        if (f.Orientation() == TopAbs_REVERSED) {
            normal.Reverse();
        }

        *nx = normal.X();
        *ny = normal.Y();
        *nz = normal.Z();

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_get_face_center(occt_shape_t face,
                         double* x, double* y, double* z,
                         char** out_error)
{
    if (!face) {
        if (out_error) *out_error = strdup("Null input face");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!x || !y || !z) {
        if (out_error) *out_error = strdup("Null output parameters");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(face);

        // Check that it's actually a face
        if (s->ShapeType() != TopAbs_FACE) {
            if (out_error) *out_error = strdup("Shape is not a face");
            return OCCT_ERROR_DOMAIN;
        }

        TopoDS_Face f = TopoDS::Face(*s);

        // Get UV bounds
        double umin, umax, vmin, vmax;
        BRepTools::UVBounds(f, umin, umax, vmin, vmax);

        // Evaluate at center of parameter space
        double u = (umin + umax) / 2.0;
        double v = (vmin + vmax) / 2.0;

        // Create surface adaptor and get point
        BRepAdaptor_Surface surface(f);
        gp_Pnt center_point = surface.Value(u, v);

        *x = center_point.X();
        *y = center_point.Y();
        *z = center_point.Z();

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_get_edge_geom_type(occt_shape_t edge, int* geom_type,
                            char** out_error)
{
    if (!edge) {
        if (out_error) *out_error = strdup("Null input edge");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!geom_type) {
        if (out_error) *out_error = strdup("Null output parameter");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(edge);

        // Check that it's actually an edge
        if (s->ShapeType() != TopAbs_EDGE) {
            if (out_error) *out_error = strdup("Shape is not an edge");
            return OCCT_ERROR_DOMAIN;
        }

        TopoDS_Edge e = TopoDS::Edge(*s);

        // Create curve adaptor
        BRepAdaptor_Curve curve(e);

        // Get curve type
        GeomAbs_CurveType curveType = curve.GetType();

        // Map to integer codes
        // 0=line, 1=circle, 2=ellipse, 3=hyperbola, 4=parabola,
        // 5=bezier, 6=bspline, 7=other
        switch (curveType) {
            case GeomAbs_Line:
                *geom_type = 0;
                break;
            case GeomAbs_Circle:
                *geom_type = 1;
                break;
            case GeomAbs_Ellipse:
                *geom_type = 2;
                break;
            case GeomAbs_Hyperbola:
                *geom_type = 3;
                break;
            case GeomAbs_Parabola:
                *geom_type = 4;
                break;
            case GeomAbs_BezierCurve:
                *geom_type = 5;
                break;
            case GeomAbs_BSplineCurve:
                *geom_type = 6;
                break;
            default:
                *geom_type = 7;  // other
                break;
        }

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_get_face_geom_type(occt_shape_t face, int* geom_type,
                            char** out_error)
{
    if (!face) {
        if (out_error) *out_error = strdup("Null input face");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!geom_type) {
        if (out_error) *out_error = strdup("Null output parameter");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(face);

        // Check that it's actually a face
        if (s->ShapeType() != TopAbs_FACE) {
            if (out_error) *out_error = strdup("Shape is not a face");
            return OCCT_ERROR_DOMAIN;
        }

        TopoDS_Face f = TopoDS::Face(*s);

        // Create surface adaptor
        BRepAdaptor_Surface surface(f);

        // Get surface type
        GeomAbs_SurfaceType surfaceType = surface.GetType();

        // Map to integer codes
        // 0=plane, 1=cylinder, 2=cone, 3=sphere, 4=torus,
        // 5=bezier, 6=bspline, 7=other
        switch (surfaceType) {
            case GeomAbs_Plane:
                *geom_type = 0;
                break;
            case GeomAbs_Cylinder:
                *geom_type = 1;
                break;
            case GeomAbs_Cone:
                *geom_type = 2;
                break;
            case GeomAbs_Sphere:
                *geom_type = 3;
                break;
            case GeomAbs_Torus:
                *geom_type = 4;
                break;
            case GeomAbs_BezierSurface:
                *geom_type = 5;
                break;
            case GeomAbs_BSplineSurface:
                *geom_type = 6;
                break;
            default:
                *geom_type = 7;  // other
                break;
        }

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

/*
 * Advanced Features - Fillets
 */

int occt_make_fillet(occt_shape_t shape,
                     occt_shape_t* edges,
                     int num_edges,
                     double radius,
                     occt_shape_t* out_shape,
                     char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!edges || num_edges <= 0) {
        if (out_error) *out_error = strdup("Invalid edges array");
        return OCCT_ERROR_DOMAIN;
    }
    if (radius <= 0.0) {
        if (out_error) *out_error = strdup("Fillet radius must be positive");
        return OCCT_ERROR_DOMAIN;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        // Create fillet builder
        BRepFilletAPI_MakeFillet filletMaker(*s);

        // Add edges to fillet operation
        for (int i = 0; i < num_edges; i++) {
            if (!edges[i]) {
                if (out_error) *out_error = strdup("Null edge in array");
                return OCCT_ERROR_NULL_OBJECT;
            }

            TopoDS_Shape* edge_shape = static_cast<TopoDS_Shape*>(edges[i]);

            // Check that it's actually an edge
            if (edge_shape->ShapeType() != TopAbs_EDGE) {
                if (out_error) *out_error = strdup("Shape in array is not an edge");
                return OCCT_ERROR_DOMAIN;
            }

            TopoDS_Edge edge = TopoDS::Edge(*edge_shape);
            filletMaker.Add(radius, edge);
        }

        // Build the filleted shape
        filletMaker.Build();

        if (!filletMaker.IsDone()) {
            if (out_error) *out_error = strdup("Fillet operation failed");
            return OCCT_ERROR_CONSTRUCTION;
        }

        TopoDS_Shape* result = new TopoDS_Shape(filletMaker.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_make_chamfer(occt_shape_t shape,
                      occt_shape_t* edges,
                      int num_edges,
                      double distance,
                      occt_shape_t* out_shape,
                      char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!edges || num_edges <= 0) {
        if (out_error) *out_error = strdup("Invalid edges array");
        return OCCT_ERROR_DOMAIN;
    }
    if (distance <= 0.0) {
        if (out_error) *out_error = strdup("Chamfer distance must be positive");
        return OCCT_ERROR_DOMAIN;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        // Create chamfer builder
        BRepFilletAPI_MakeChamfer chamferMaker(*s);

        // Add edges to chamfer operation
        for (int i = 0; i < num_edges; i++) {
            if (!edges[i]) {
                if (out_error) *out_error = strdup("Null edge in array");
                return OCCT_ERROR_NULL_OBJECT;
            }

            TopoDS_Shape* edge_shape = static_cast<TopoDS_Shape*>(edges[i]);

            // Check that it's actually an edge
            if (edge_shape->ShapeType() != TopAbs_EDGE) {
                if (out_error) *out_error = strdup("Shape in array is not an edge");
                return OCCT_ERROR_DOMAIN;
            }

            TopoDS_Edge edge = TopoDS::Edge(*edge_shape);

            // Add symmetric chamfer (OCCT automatically handles adjacent faces)
            chamferMaker.Add(distance, edge);
        }

        // Build the chamfered shape
        chamferMaker.Build();

        if (!chamferMaker.IsDone()) {
            if (out_error) *out_error = strdup("Chamfer operation failed");
            return OCCT_ERROR_CONSTRUCTION;
        }

        TopoDS_Shape* result = new TopoDS_Shape(chamferMaker.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

/*
 * Memory Management
 */

void occt_release_shape(occt_shape_t shape)
{
    if (shape) {
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);
        delete s;
    }
}

int occt_copy_shape(occt_shape_t shape,
                    occt_shape_t* out_shape, char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);
        TopoDS_Shape* copy = new TopoDS_Shape(*s);
        *out_shape = copy;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_shape_is_null(occt_shape_t shape)
{
    if (!shape) return 1;

    TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);
    return s->IsNull() ? 1 : 0;
}

/*
 * Advanced Features - Curves and Splines
 */

int occt_make_interpolated_curve(const double* points, int num_points,
                                   int closed,
                                   occt_shape_t* out_edge,
                                   char** out_error)
{
    if (!out_edge) return OCCT_ERROR_NULL_OBJECT;
    if (!points || num_points < 2) {
        if (out_error) *out_error = strdup("Need at least 2 points for curve");
        return OCCT_ERROR_DOMAIN;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        // Create array of points
        TColgp_Array1OfPnt pnt_array(1, num_points);
        for (int i = 0; i < num_points; i++) {
            int idx = i * 3;
            pnt_array(i + 1) = gp_Pnt(points[idx], points[idx + 1], points[idx + 2]);
        }

        // Create interpolated B-spline
        Handle(Geom_BSplineCurve) curve;
        GeomAPI_PointsToBSpline interpolator(pnt_array);
        
        if (!interpolator.IsDone()) {
            if (out_error) *out_error = strdup("Failed to create interpolated curve");
            return OCCT_ERROR_CONSTRUCTION;
        }
        
        curve = interpolator.Curve();
        
        // Create edge from curve
        BRepBuilderAPI_MakeEdge edgeMaker(curve);
        
        if (!edgeMaker.IsDone()) {
            if (out_error) *out_error = strdup("Failed to create edge from curve");
            return OCCT_ERROR_CONSTRUCTION;
        }
        
        TopoDS_Shape* result = new TopoDS_Shape(edgeMaker.Edge());
        *out_edge = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_make_bezier_curve(const double* control_points, int num_points,
                            occt_shape_t* out_edge,
                            char** out_error)
{
    if (!out_edge) return OCCT_ERROR_NULL_OBJECT;
    if (!control_points || num_points < 2 || num_points > 25) {
        if (out_error) *out_error = strdup("Bezier curve requires 2-25 control points");
        return OCCT_ERROR_DOMAIN;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        // Create array of control points
        TColgp_Array1OfPnt pnt_array(1, num_points);
        for (int i = 0; i < num_points; i++) {
            int idx = i * 3;
            pnt_array(i + 1) = gp_Pnt(control_points[idx], control_points[idx + 1], control_points[idx + 2]);
        }

        // Create Bezier curve
        Handle(Geom_BezierCurve) curve = new Geom_BezierCurve(pnt_array);
        
        // Create edge from curve
        BRepBuilderAPI_MakeEdge edgeMaker(curve);
        
        if (!edgeMaker.IsDone()) {
            if (out_error) *out_error = strdup("Failed to create edge from Bezier curve");
            return OCCT_ERROR_CONSTRUCTION;
        }
        
        TopoDS_Shape* result = new TopoDS_Shape(edgeMaker.Edge());
        *out_edge = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_make_arc_3points(double x1, double y1, double z1,
                           double x2, double y2, double z2,
                           double x3, double y3, double z3,
                           occt_shape_t* out_edge,
                           char** out_error)
{
    if (!out_edge) return OCCT_ERROR_NULL_OBJECT;
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        gp_Pnt p1(x1, y1, z1);
        gp_Pnt p2(x2, y2, z2);
        gp_Pnt p3(x3, y3, z3);
        
        // Create circular arc through 3 points
        GC_MakeArcOfCircle arcMaker(p1, p2, p3);
        
        if (!arcMaker.IsDone()) {
            if (out_error) *out_error = strdup("Failed to create arc through 3 points");
            return OCCT_ERROR_CONSTRUCTION;
        }
        
        Handle(Geom_TrimmedCurve) arc = arcMaker.Value();
        
        // Create edge from arc
        BRepBuilderAPI_MakeEdge edgeMaker(arc);
        
        if (!edgeMaker.IsDone()) {
            if (out_error) *out_error = strdup("Failed to create edge from arc");
            return OCCT_ERROR_CONSTRUCTION;
        }
        
        TopoDS_Shape* result = new TopoDS_Shape(edgeMaker.Edge());
        *out_edge = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_make_arc_center_radius(double cx, double cy, double cz,
                                 double radius,
                                 double start_angle, double end_angle,
                                 double axis_x, double axis_y, double axis_z,
                                 occt_shape_t* out_edge,
                                 char** out_error)
{
    if (!out_edge) return OCCT_ERROR_NULL_OBJECT;
    if (radius <= 0.0) {
        if (out_error) *out_error = strdup("Arc radius must be positive");
        return OCCT_ERROR_DOMAIN;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        gp_Pnt center(cx, cy, cz);
        gp_Dir axis(axis_x, axis_y, axis_z);
        
        // Create coordinate system
        gp_Ax2 ax2(center, axis);
        
        // Create circle
        gp_Circ circle(ax2, radius);
        
        // Convert angles to radians
        double start_rad = start_angle * M_PI / 180.0;
        double end_rad = end_angle * M_PI / 180.0;
        
        // Create arc
        Handle(Geom_TrimmedCurve) arc = GC_MakeArcOfCircle(circle, start_rad, end_rad, Standard_True).Value();
        
        // Create edge from arc
        BRepBuilderAPI_MakeEdge edgeMaker(arc);
        
        if (!edgeMaker.IsDone()) {
            if (out_error) *out_error = strdup("Failed to create edge from arc");
            return OCCT_ERROR_CONSTRUCTION;
        }
        
        TopoDS_Shape* result = new TopoDS_Shape(edgeMaker.Edge());
        *out_edge = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_make_wire(occt_shape_t* edges, int num_edges,
                   occt_shape_t* out_wire,
                   char** out_error)
{
    if (!out_wire) return OCCT_ERROR_NULL_OBJECT;
    if (!edges || num_edges <= 0) {
        if (out_error) *out_error = strdup("Invalid edges array");
        return OCCT_ERROR_DOMAIN;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        BRepBuilderAPI_MakeWire wireMaker;
        
        // Add edges to wire
        for (int i = 0; i < num_edges; i++) {
            if (!edges[i]) {
                if (out_error) *out_error = strdup("Null edge in array");
                return OCCT_ERROR_NULL_OBJECT;
            }
            
            TopoDS_Shape* edge_shape = static_cast<TopoDS_Shape*>(edges[i]);
            
            if (edge_shape->ShapeType() != TopAbs_EDGE) {
                if (out_error) *out_error = strdup("Shape in array is not an edge");
                return OCCT_ERROR_DOMAIN;
            }
            
            TopoDS_Edge edge = TopoDS::Edge(*edge_shape);
            wireMaker.Add(edge);
        }
        
        // Build the wire
        if (!wireMaker.IsDone()) {
            if (out_error) *out_error = strdup("Failed to create wire from edges");
            return OCCT_ERROR_CONSTRUCTION;
        }
        
        TopoDS_Shape* result = new TopoDS_Shape(wireMaker.Wire());
        *out_wire = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_wire_is_closed(occt_shape_t wire, int* is_closed, char** out_error)
{
    if (!wire) {
        if (out_error) *out_error = strdup("Null wire");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (!is_closed) {
        if (out_error) *out_error = strdup("Null output parameter");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(wire);

        if (s->ShapeType() != TopAbs_WIRE) {
            if (out_error) *out_error = strdup("Shape is not a wire");
            return OCCT_ERROR_DOMAIN;
        }

        TopoDS_Wire w = TopoDS::Wire(*s);
        *is_closed = BRep_Tool::IsClosed(w) ? 1 : 0;

        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

/*
 * Advanced Features - Sweeps
 */

int occt_make_pipe(occt_shape_t profile, occt_shape_t path,
                   occt_shape_t* out_shape,
                   char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!profile || !path) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* profile_shape = static_cast<TopoDS_Shape*>(profile);
        TopoDS_Shape* path_shape = static_cast<TopoDS_Shape*>(path);

        // Path should be a wire or edge
        TopoDS_Wire path_wire;
        if (path_shape->ShapeType() == TopAbs_WIRE) {
            path_wire = TopoDS::Wire(*path_shape);
        } else if (path_shape->ShapeType() == TopAbs_EDGE) {
            BRepBuilderAPI_MakeWire wireMaker(TopoDS::Edge(*path_shape));
            path_wire = wireMaker.Wire();
        } else {
            if (out_error) *out_error = strdup("Path must be a wire or edge");
            return OCCT_ERROR_DOMAIN;
        }

        // Create pipe
        BRepOffsetAPI_MakePipe pipeMaker(path_wire, *profile_shape);
        pipeMaker.Build();

        if (!pipeMaker.IsDone()) {
            if (out_error) *out_error = strdup("Pipe operation failed");
            return OCCT_ERROR_CONSTRUCTION;
        }

        TopoDS_Shape* result = new TopoDS_Shape(pipeMaker.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

int occt_make_pipe_shell(occt_shape_t path, double radius,
                         occt_shape_t* out_shape,
                         char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!path) {
        if (out_error) *out_error = strdup("Null path");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (radius <= 0.0) {
        if (out_error) *out_error = strdup("Radius must be positive");
        return OCCT_ERROR_DOMAIN;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* path_shape = static_cast<TopoDS_Shape*>(path);

        // Path should be a wire or edge
        TopoDS_Wire path_wire;
        if (path_shape->ShapeType() == TopAbs_WIRE) {
            path_wire = TopoDS::Wire(*path_shape);
        } else if (path_shape->ShapeType() == TopAbs_EDGE) {
            BRepBuilderAPI_MakeWire wireMaker(TopoDS::Edge(*path_shape));
            path_wire = wireMaker.Wire();
        } else {
            if (out_error) *out_error = strdup("Path must be a wire or edge");
            return OCCT_ERROR_DOMAIN;
        }

        // Create circular profile
        // Get first point of wire to position circle
        TopExp_Explorer exp(path_wire, TopAbs_EDGE);
        if (!exp.More()) {
            if (out_error) *out_error = strdup("Path wire has no edges");
            return OCCT_ERROR_DOMAIN;
        }

        TopoDS_Edge first_edge = TopoDS::Edge(exp.Current());
        double first, last;
        Handle(Geom_Curve) curve = BRep_Tool::Curve(first_edge, first, last);
        gp_Pnt start_point = curve->Value(first);

        // Create a circle perpendicular to path start
        gp_Vec tangent;
        gp_Pnt pnt;
        curve->D1(first, pnt, tangent);
        gp_Dir path_dir(tangent);

        gp_Ax2 ax2(start_point, path_dir);
        gp_Circ circle(ax2, radius);

        TopoDS_Edge circle_edge = BRepBuilderAPI_MakeEdge(circle);
        TopoDS_Wire circle_wire = BRepBuilderAPI_MakeWire(circle_edge).Wire();

        // Create pipe
        BRepOffsetAPI_MakePipe pipeMaker(path_wire, circle_wire);
        pipeMaker.Build();

        if (!pipeMaker.IsDone()) {
            if (out_error) *out_error = strdup("Pipe shell operation failed");
            return OCCT_ERROR_CONSTRUCTION;
        }

        TopoDS_Shape* result = new TopoDS_Shape(pipeMaker.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

/*
 * Advanced Features - Lofts
 */

int occt_make_loft(occt_shape_t* sections, int num_sections,
                   int solid, int ruled,
                   occt_shape_t* out_shape,
                   char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!sections || num_sections < 2) {
        if (out_error) *out_error = strdup("Need at least 2 sections for loft");
        return OCCT_ERROR_DOMAIN;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        // Create loft builder
        BRepOffsetAPI_ThruSections loftMaker(solid ? Standard_True : Standard_False,
                                             ruled ? Standard_True : Standard_False);

        // Add sections
        for (int i = 0; i < num_sections; i++) {
            if (!sections[i]) {
                if (out_error) *out_error = strdup("Null section in array");
                return OCCT_ERROR_NULL_OBJECT;
            }

            TopoDS_Shape* section_shape = static_cast<TopoDS_Shape*>(sections[i]);

            // Convert to wire if needed
            TopoDS_Wire section_wire;
            if (section_shape->ShapeType() == TopAbs_WIRE) {
                section_wire = TopoDS::Wire(*section_shape);
            } else if (section_shape->ShapeType() == TopAbs_EDGE) {
                BRepBuilderAPI_MakeWire wireMaker(TopoDS::Edge(*section_shape));
                section_wire = wireMaker.Wire();
            } else if (section_shape->ShapeType() == TopAbs_FACE) {
                // Extract outer wire from face
                TopoDS_Face face = TopoDS::Face(*section_shape);
                section_wire = BRepTools::OuterWire(face);
            } else {
                if (out_error) *out_error = strdup("Section must be wire, edge, or face");
                return OCCT_ERROR_DOMAIN;
            }

            loftMaker.AddWire(section_wire);
        }

        // Build the loft
        loftMaker.Build();

        if (!loftMaker.IsDone()) {
            if (out_error) *out_error = strdup("Loft operation failed");
            return OCCT_ERROR_CONSTRUCTION;
        }

        TopoDS_Shape* result = new TopoDS_Shape(loftMaker.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

/*
 * Advanced Features - Shelling
 */

int occt_make_shell(occt_shape_t solid,
                    occt_shape_t* faces_to_remove,
                    int num_faces,
                    double thickness,
                    double tolerance,
                    occt_shape_t* out_shape,
                    char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!solid) {
        if (out_error) *out_error = strdup("Null solid");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (thickness == 0.0) {
        if (out_error) *out_error = strdup("Thickness cannot be zero");
        return OCCT_ERROR_DOMAIN;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* solid_shape = static_cast<TopoDS_Shape*>(solid);

        // Create list of faces to remove
        TopTools_ListOfShape faces_list;
        if (faces_to_remove && num_faces > 0) {
            for (int i = 0; i < num_faces; i++) {
                if (!faces_to_remove[i]) {
                    if (out_error) *out_error = strdup("Null face in array");
                    return OCCT_ERROR_NULL_OBJECT;
                }

                TopoDS_Shape* face_shape = static_cast<TopoDS_Shape*>(faces_to_remove[i]);

                if (face_shape->ShapeType() != TopAbs_FACE) {
                    if (out_error) *out_error = strdup("Shape in array is not a face");
                    return OCCT_ERROR_DOMAIN;
                }

                faces_list.Append(*face_shape);
            }
        }

        // Create shelled solid
        BRepOffsetAPI_MakeThickSolid shellMaker;
        shellMaker.MakeThickSolidByJoin(*solid_shape, faces_list, thickness, tolerance);

        if (!shellMaker.IsDone()) {
            if (out_error) *out_error = strdup("Shelling operation failed");
            return OCCT_ERROR_CONSTRUCTION;
        }

        TopoDS_Shape* result = new TopoDS_Shape(shellMaker.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

/*
 * Advanced Features - Mirroring
 */

int occt_mirror_shape(occt_shape_t shape,
                      double plane_ox, double plane_oy, double plane_oz,
                      double plane_nx, double plane_ny, double plane_nz,
                      occt_shape_t* out_shape,
                      char** out_error)
{
    if (!out_shape) return OCCT_ERROR_NULL_OBJECT;
    if (!shape) {
        if (out_error) *out_error = strdup("Null input shape");
        return OCCT_ERROR_NULL_OBJECT;
    }
    if (out_error) *out_error = nullptr;

    TRY_CATCH_BEGIN
        TopoDS_Shape* s = static_cast<TopoDS_Shape*>(shape);

        // Create mirror plane
        gp_Pnt origin(plane_ox, plane_oy, plane_oz);
        gp_Dir normal(plane_nx, plane_ny, plane_nz);
        gp_Ax2 plane(origin, normal);

        // Create mirror transformation
        gp_Trsf transformation;
        transformation.SetMirror(plane);

        // Apply transformation
        BRepBuilderAPI_Transform transformer(*s, transformation, Standard_True);
        TopoDS_Shape* result = new TopoDS_Shape(transformer.Shape());
        *out_shape = result;
        return OCCT_SUCCESS;
    TRY_CATCH_END(out_error)
}

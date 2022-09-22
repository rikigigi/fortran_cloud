project(fortran_cloud LANGUAGES C Fortran VERSION 0.8.0 )
add_subdirectory(libzmq) #Adds library ZEROMQ_LIBRARY

CONFIGURE_FILE(fzmq/src/zmq_constants.c.in zmq_constants.c)
SET_SOURCE_FILES_PROPERTIES(${CMAKE_CURRENT_BINARY_DIR}/zmq_constants.c GENERATED)


function(fortran_modules TGT)
    set(targets ${TGT} ${ARGN})
    foreach(tgt IN LISTS targets)
        get_target_property(tgt_type ${tgt} TYPE)
        # All of the following target modifications make
        # sense on non-interfaces only
        if(NOT ${tgt_type} STREQUAL "INTERFACE_LIBRARY")
            get_target_property(tgt_module_dir ${tgt} Fortran_MODULE_DIRECTORY)
            # set module path to tgt_binary_dir/mod
            get_target_property(tgt_binary_dir ${tgt} BINARY_DIR)
            set_target_properties(${tgt}
                PROPERTIES
                    Fortran_MODULE_DIRECTORY ${tgt_binary_dir}/mod/${TGT})
            # make module directory available for clients of TGT 
            target_include_directories(${tgt}
                PUBLIC
                    $<BUILD_INTERFACE:${tgt_binary_dir}/mod/${TGT}>
                INTERFACE
                    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME}/${TGT}>)
        endif()
    endforeach()
endfunction(qe_fix_fortran_modules)



set(LIB_SRC
    cloud/cloud_0mq.F90
    ${CMAKE_CURRENT_BINARY_DIR}/zmq_constants.c
    fzmq/src/zmq.f90
)
add_library(${PROJECT_NAME} STATIC ${LIB_SRC})
target_link_libraries(${PROJECT_NAME}
	PUBLIC 
	   ${ZEROMQ_LIBRARY}
        )

fortran_modules(${PROJECT_NAME})

INSTALL(TARGETS ${PROJECT_NAME}
        EXPORT "${PROJECT_NAME}Targets"
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}	
	)

install(EXPORT "${PROJECT_NAME}Targets"
        FILE "${PROJECT_NAME}Targets.cmake"
        NAMESPACE fortran_cloud::
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortran_cloud
)

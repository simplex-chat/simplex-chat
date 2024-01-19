# Manually downloaded headers because `find_package(JNI REQUIRED)` doesn't work while cross-compiling:

# https://raw.githubusercontent.com/openjdk/jdk/master/src/java.base/share/native/include/jni.h
include_directories(${CMAKE_SOURCE_DIR}/include)
if(UNIX)
    # https://raw.githubusercontent.com/openjdk/jdk/master/src/java.base/unix/native/include/jni_md.h
    include_directories(${CMAKE_SOURCE_DIR}/include/unix)
elseif(WIN32)
    # https://raw.githubusercontent.com/openjdk/jdk/master/src/java.base/windows/native/include/jni_md.h
    include_directories(${CMAKE_SOURCE_DIR}/include/windows)
endif()

# Sets the minimum version of CMake required to build the native library.
cmake_minimum_required(VERSION 3.10.2)

# Declares and names the project.
project("app")

if(UNIX AND NOT APPLE)
    set(OS_LIB_PATH "linux")
    set(OS_LIB_EXT "so")
    # Makes ld search libs in the same dir as libapp-lib, not in system dirs
    set(CMAKE_BUILD_RPATH "$ORIGIN")
elseif(WIN32)
    set(OS_LIB_PATH "windows")
    set(OS_LIB_EXT "dll")
else()
    set(OS_LIB_PATH "mac")
    set(OS_LIB_EXT "dylib")
    set(CMAKE_MACOSX_RPATH 1)
    set(CMAKE_BUILD_RPATH "@loader_path")
endif()

if(${CMAKE_SYSTEM_PROCESSOR} MATCHES "amd64" OR ${CMAKE_SYSTEM_PROCESSOR} MATCHES "AMD64")
    set(OS_LIB_ARCH "x86_64")
elseif(${CMAKE_SYSTEM_PROCESSOR} MATCHES "arm64" OR ${CMAKE_SYSTEM_PROCESSOR} MATCHES "ARM64")
    set(OS_LIB_ARCH "aarch64")
else()
    set(OS_LIB_ARCH "${CMAKE_SYSTEM_PROCESSOR}")
endif()

# Creates and names a library, sets it as either STATIC
# or SHARED, and provides the relative paths to its source code.
# You can define multiple libraries, and CMake builds them for you.
# Gradle automatically packages shared libraries with your APK.

add_library( # Sets the name of the library.
    app-lib

    # Sets the library as a shared library.
    SHARED

    # Provides a relative path to your source file(s).
    simplex-api.c)

add_library( simplex SHARED IMPORTED )
if(WIN32)
    FILE(GLOB SIMPLEXLIB ${CMAKE_SOURCE_DIR}/libs/${OS_LIB_PATH}-${OS_LIB_ARCH}/lib*simplex*.${OS_LIB_EXT})
    set_target_properties( simplex PROPERTIES IMPORTED_IMPLIB ${SIMPLEXLIB})
else()
    FILE(GLOB SIMPLEXLIB ${CMAKE_SOURCE_DIR}/libs/${OS_LIB_PATH}-${OS_LIB_ARCH}/lib*simplex-chat*.${OS_LIB_EXT})
    set_target_properties( simplex PROPERTIES IMPORTED_LOCATION ${SIMPLEXLIB})
endif()


# Specifies libraries CMake should link to your target library. You
# can link multiple libraries, such as libraries you define in this
# build script, prebuilt third-party libraries, or system libraries.
if(NOT APPLE)
	target_link_libraries(app-lib simplex)
else()
	# Without direct linking it can't find hs_init in linking step
	add_library( rts SHARED IMPORTED )
	FILE(GLOB RTSLIB ${CMAKE_SOURCE_DIR}/libs/${OS_LIB_PATH}-${OS_LIB_ARCH}/libHSrts*_thr-*.${OS_LIB_EXT})
	set_target_properties( rts PROPERTIES IMPORTED_LOCATION ${RTSLIB})

	target_link_libraries(app-lib rts simplex)
endif()

if(APPLE)
    add_custom_command(TARGET app-lib POST_BUILD
        COMMAND  ${CMAKE_CURRENT_SOURCE_DIR}/patch-libapp-mac.sh
)
endif()


# Trying to copy resulting files into needed directory, but none of these work for some reason. This could allow to
# remove gradle's copy hooks
#add_custom_target(lalal)
#MACRO(POST_BUILD_COPY src_files dest_path)
#    ADD_CUSTOM_COMMAND(TARGET lalal POST_BUILD
#        COMMAND ${CMAKE_COMMAND} -E copy ${src_files} ${dest_path})
#ENDMACRO(POST_BUILD_COPY src_files dest_path)

#file(COPY "${CMAKE_SOURCE_DIR}/libs/${OS_LIB_PATH}-${OS_LIB_ARCH}/libsimplex.${OS_LIB_EXT}" DESTINATION "../../resources/libs/${OS_LIB_PATH}-${OS_LIB_ARCH}/libsimplex.${OS_LIB_EXT}")
#POST_BUILD_COPY("${CMAKE_BINARY_DIR}/libapp-lib.${OS_LIB_EXT}" "../resources/libs/${OS_LIB_PATH}-${OS_LIB_ARCH}/")

#add_custom_command(TARGET lalal POST_BUILD
#    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/libapp-lib.${OS_LIB_EXT} "../../resources/libs/${OS_LIB_PATH}-${OS_LIB_ARCH}/libapp-lib.${OS_LIB_EXT}"
#)

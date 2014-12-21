# only in the development code we get the hash from git
# in the released code we read it from file, in this case
# it is already set at this stage
if(DEVELOPMENT_CODE)
    find_package(Git)
    if(GIT_FOUND)
        execute_process(
            COMMAND ${GIT_EXECUTABLE} rev-list --max-count=1 HEAD
            OUTPUT_VARIABLE GIT_REVISION
            ERROR_QUIET
            )
        if(NOT ${GIT_REVISION} STREQUAL "")
            string(STRIP ${GIT_REVISION} GIT_REVISION)
        endif()

      # radovan: problematic on machines with old git
      # execute_process(
      #     COMMAND ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
      #     OUTPUT_VARIABLE GIT_BRANCH
      #     ERROR_QUIET
      #     )
      
      if(CMAKE_SYSTEM_NAME MATCHES "Windows")
        execute_process(
              COMMAND powershell.exe "${GIT_EXECUTABLE} branch | findstr *"
              OUTPUT_VARIABLE GIT_BRANCH
              ERROR_QUIET
              )
      else()
          execute_process(
              COMMAND ${GIT_EXECUTABLE} branch
              COMMAND grep "*"
              OUTPUT_VARIABLE GIT_BRANCH
              ERROR_QUIET
              )
      endif()
      if(NOT ${GIT_BRANCH} STREQUAL "")
          string(REPLACE "*" " " GIT_BRANCH ${GIT_BRANCH})
          string(STRIP ${GIT_BRANCH} GIT_BRANCH)
          message("-- GIT_BRANCH            : ${GIT_BRANCH}")
      endif()

      execute_process(
          COMMAND     ${GIT_EXECUTABLE} status -uno
          OUTPUT_FILE ${PROJECT_BINARY_DIR}/GIT_STATUS_AT_BUILD
          ERROR_QUIET
          )

    endif()
endif()

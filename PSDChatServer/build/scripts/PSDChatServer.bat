@if "%DEBUG%" == "" @echo off
@rem ##########################################################################
@rem
@rem  PSDChatServer startup script for Windows
@rem
@rem ##########################################################################

@rem Set local scope for the variables with windows NT shell
if "%OS%"=="Windows_NT" setlocal

@rem Add default JVM options here. You can also use JAVA_OPTS and PSD_CHAT_SERVER_OPTS to pass JVM options to this script.
set DEFAULT_JVM_OPTS=

set DIRNAME=%~dp0
if "%DIRNAME%" == "" set DIRNAME=.
set APP_BASE_NAME=%~n0
set APP_HOME=%DIRNAME%..

@rem Find java.exe
if defined JAVA_HOME goto findJavaFromJavaHome

set JAVA_EXE=java.exe
%JAVA_EXE% -version >NUL 2>&1
if "%ERRORLEVEL%" == "0" goto init

echo.
echo ERROR: JAVA_HOME is not set and no 'java' command could be found in your PATH.
echo.
echo Please set the JAVA_HOME variable in your environment to match the
echo location of your Java installation.

goto fail

:findJavaFromJavaHome
set JAVA_HOME=%JAVA_HOME:"=%
set JAVA_EXE=%JAVA_HOME%/bin/java.exe

if exist "%JAVA_EXE%" goto init

echo.
echo ERROR: JAVA_HOME is set to an invalid directory: %JAVA_HOME%
echo.
echo Please set the JAVA_HOME variable in your environment to match the
echo location of your Java installation.

goto fail

:init
@rem Get command-line arguments, handling Windowz variants

if not "%OS%" == "Windows_NT" goto win9xME_args
if "%@eval[2+2]" == "4" goto 4NT_args

:win9xME_args
@rem Slurp the command line arguments.
set CMD_LINE_ARGS=
set _SKIP=2

:win9xME_args_slurp
if "x%~1" == "x" goto execute

set CMD_LINE_ARGS=%*
goto execute

:4NT_args
@rem Get arguments from the 4NT Shell from JP Software
set CMD_LINE_ARGS=%$

:execute
@rem Setup the command line

set CLASSPATH=%APP_HOME%\lib\PSDChatServer.jar;%APP_HOME%\lib\quasar-core-0.7.3-jdk8.jar;%APP_HOME%\lib\quasar-actors-0.7.3.jar;%APP_HOME%\lib\quasar-galaxy-0.7.3.jar;%APP_HOME%\lib\quasar-reactive-streams-0.7.3.jar;%APP_HOME%\lib\quasar-kotlin-0.7.3.jar;%APP_HOME%\lib\metrics-core-3.1.2.jar;%APP_HOME%\lib\LatencyUtils-2.0.2.jar;%APP_HOME%\lib\kryo-serializers-0.36.jar;%APP_HOME%\lib\HdrHistogram-2.1.3.jar;%APP_HOME%\lib\disruptor-3.3.2.jar;%APP_HOME%\lib\guava-18.0.jar;%APP_HOME%\lib\kryo-2.24.0.jar;%APP_HOME%\lib\byte-buddy-0.6.15.jar;%APP_HOME%\lib\galaxy-1.4.jar;%APP_HOME%\lib\reactive-streams-1.0.0.jar;%APP_HOME%\lib\kotlin-reflect-0.12.613.jar;%APP_HOME%\lib\kotlin-stdlib-0.12.613.jar;%APP_HOME%\lib\protobuf-java-2.6.1.jar;%APP_HOME%\lib\minlog-1.2.jar;%APP_HOME%\lib\objenesis-2.1.jar;%APP_HOME%\lib\metrics-core-3.0.2.jar;%APP_HOME%\lib\curator-client-2.6.0.jar;%APP_HOME%\lib\je-5.0.73.jar;%APP_HOME%\lib\trove4j-3.0.3.jar;%APP_HOME%\lib\spring-context-4.0.6.RELEASE.jar;%APP_HOME%\lib\zookeeper-3.4.6.jar;%APP_HOME%\lib\curator-framework-2.6.0.jar;%APP_HOME%\lib\spring-beans-4.0.6.RELEASE.jar;%APP_HOME%\lib\concurrentlinkedhashmap-lru-1.4.jar;%APP_HOME%\lib\netty-3.9.2.Final.jar;%APP_HOME%\lib\jgroups-3.4.4.Final.jar;%APP_HOME%\lib\curator-recipes-2.6.0.jar;%APP_HOME%\lib\log4j-api-2.0.jar;%APP_HOME%\lib\log4j-core-2.0.jar;%APP_HOME%\lib\log4j-slf4j-impl-2.0.jar;%APP_HOME%\lib\kotlin-runtime-0.12.613.jar;%APP_HOME%\lib\spring-aop-4.0.6.RELEASE.jar;%APP_HOME%\lib\spring-core-4.0.6.RELEASE.jar;%APP_HOME%\lib\spring-expression-4.0.6.RELEASE.jar;%APP_HOME%\lib\log4j-1.2.16.jar;%APP_HOME%\lib\jline-0.9.94.jar;%APP_HOME%\lib\aopalliance-1.0.jar;%APP_HOME%\lib\commons-logging-1.1.3.jar;%APP_HOME%\lib\slf4j-api-1.7.12.jar

@rem Execute PSDChatServer
"%JAVA_EXE%" %DEFAULT_JVM_OPTS% %JAVA_OPTS% %PSD_CHAT_SERVER_OPTS%  -classpath "%CLASSPATH%" ActorChat.ChatServerV2 %CMD_LINE_ARGS%

:end
@rem End local scope for the variables with windows NT shell
if "%ERRORLEVEL%"=="0" goto mainEnd

:fail
rem Set variable PSD_CHAT_SERVER_EXIT_CONSOLE if you need the _script_ return code instead of
rem the _cmd.exe /c_ return code!
if  not "" == "%PSD_CHAT_SERVER_EXIT_CONSOLE%" exit 1
exit /b 1

:mainEnd
if "%OS%"=="Windows_NT" endlocal

:omega

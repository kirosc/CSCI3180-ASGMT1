cmake_minimum_required(VERSION 3.15)
project(CSCI3180_ASGMT1 C)

set(CMAKE_C_STANDARD 99)

set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/..)

add_executable(CSCI3180_ASGMT1 ta_ranking.c)

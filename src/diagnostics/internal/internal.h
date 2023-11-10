// Copyright 2023 Pontus Henriksson & Neo Mannskär

#pragma once
#include <iostream>
#include <chrono>
#include <string>

namespace internal {
    static std::string error(const std::chrono::high_resolution_clock::time_point& time, std::string header, const std::string &description, const std::string &desc_var = "", bool optional_terminator = true) {
        std::string time_spacing = std::to_string(std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - time).count());
        if (time_spacing.length() < 6) {
            time_spacing = std::string(6 - time_spacing.length(), ' ') + time_spacing;
        }
        std::string terminator = optional_terminator ? "! process terminated" : "";

        /*
        std::cerr << "lotus ~ internal error: " << header << "\n"
                  << time_spacing << description << desc_var << "\n"
                  << std::string.ltsu(time_spacing.length(), ' ') << "\t " << line
                  << HERE WE HAVE ERROR POINTER
                  << std::string.ltsu(time_spacing.length(), ' ') << terminator << "\n";
        */
        return "lotus ~ internal error: " + header + "\n" + time_spacing + description + desc_var + "\n" + std::string(time_spacing.length(), ' ') + terminator + "\n";
    }
}  // namespace internal

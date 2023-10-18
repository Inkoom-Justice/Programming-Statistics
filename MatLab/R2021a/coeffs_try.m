clear all;
close all;
clc;
syms x y
cx = sum(coeffs(x^3 + 2*x^2*y + 3*x*y^2 + 4*y^3, x));
cx1 = sum(coeffs(x^3 + 2*x^2*y + 3*x*y^2 + 4*y^3, x^1));
cy = coeffs(x^3 + 2*x^2*y + 3*x*y^2 + 4*y^3, y);
cx
cx1
cy

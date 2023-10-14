'use strict';

const del = require('delete');
const gulp = require('gulp');
const less = require('gulp-less');

const imageSrcDir = 'src/main/assets/images';

const paths = {
  css: {
    src: [ 'src/main/assets/styles/**/*.less', 'src/main/assets/styles/**/*.css' ],
    dest: 'dist/css'
  },
  html: {
    src: [ 'src/main/assets/html/*.html' ],
    dest: 'dist'
  },
  img: {
    src: [ imageSrcDir + '/*.jpg',
	   imageSrcDir + '/*.png',
	   imageSrcDir + '/*.gif'
	 ],
    dest: 'dist/images'
  }
};

function css() {
  return gulp.src(paths.css.src)
    .pipe(less())
    .pipe(gulp.dest(paths.css.dest));
}

function html() {
  return gulp.src(paths.html.src)
    .pipe(gulp.dest(paths.html.dest));
}

function images() {
  return gulp.src(paths.img.src)
    .pipe(gulp.dest(paths.img.dest));
}

function watchLess() {
  return gulp.watch(paths.css.src, gulp.parallel(css));
}

function watch() {
  html();
  images();
  watchLess();
}

function clean() {
  return del(['dist/**'], { force: true });
}

exports.clean = clean
exports.html = html
exports.default = watch

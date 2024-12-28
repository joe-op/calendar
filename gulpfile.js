'use strict';

const del = require('delete');
const gulp = require('gulp');
const less = require('gulp-less');
const path = require('path');

const imageSrcDir = 'src/main/assets/images';

const paths = {
  css: {
    src: [ 'src/main/assets/less/**/*.less' ],
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
    .pipe(less({
      paths: [ path.join(__dirname, 'less', 'includes') ]
    }))
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

function clean() {
  return del(['dist/**'], { force: true });
}

exports.clean = clean;
exports.css = css;
exports.html = html;
exports.static = gulp.parallel(css, html, images);
exports.default = gulp.series(clean, exports.static);

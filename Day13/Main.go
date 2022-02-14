package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type Matrix [][]bool
type Point [2]int
type Fold struct {
	foldType  byte
	foldValue int
}

func ParseFile() (Matrix, []Fold) {
	filebuffer, err := ioutil.ReadFile("input.txt")
	if err != nil {
		os.Exit(1)
	}
	data := bufio.NewScanner(strings.NewReader(string(filebuffer)))
	data.Split(bufio.ScanLines)

	var points []Point
	var folds []Fold
	columns := 0
	rows := 0
	for data.Scan() {
		line := data.Text()
		if strings.ContainsRune(line, ',') {
			coords := strings.Split(line, ",")
			x, _ := strconv.Atoi(coords[0])
			y, _ := strconv.Atoi(coords[1])
			if x > columns {
				columns = x
			}
			if y > rows {
				rows = y
			}
			points = append(points, Point{y, x})
		} else if strings.ContainsRune(line, '=') {
			sent := strings.Split(line, "=")
			value, _ := strconv.Atoi(sent[1])
			folds = append(folds, Fold{foldType: line[11], foldValue: value})
		}
	}
	matrix := make(Matrix, rows+1)
	for i := 0; i < rows+1; i++ {
		matrix[i] = make([]bool, columns+1)
	}
	for _, p := range points {
		matrix[p[0]][p[1]] = true
	}
	return matrix, folds
}

func FoldMatrix(matrix *Matrix, fold Fold) Matrix {
	rows := len(*matrix)
	columns := len((*matrix)[0])
	var folded Matrix
	if fold.foldType == 'y' {
		folded = make(Matrix, fold.foldValue)
		for i := 0; i < fold.foldValue; i++ {
			folded[i] = make([]bool, columns)
		}
		for i := 0; i < fold.foldValue; i++ {
			newRow := fold.foldValue - i - 1
			oldRow := fold.foldValue + i + 1
			if newRow >= rows || oldRow >= rows {
				continue
			}
			for j := 0; j < columns; j++ {
				folded[newRow][j] = (*matrix)[newRow][j] || (*matrix)[oldRow][j]
			}
		}
	} else {
		folded = make(Matrix, rows)
		for i := 0; i < rows; i++ {
			folded[i] = make([]bool, fold.foldValue)
		}
		for i := 0; i < rows; i++ {
			for j := 0; j < fold.foldValue; j++ {
				newCol := fold.foldValue - j - 1
				oldCol := fold.foldValue + j + 1
				folded[i][newCol] = (*matrix)[i][newCol] || (*matrix)[i][oldCol]
			}
		}
	}
	return folded
}

func main() {
	matrix, folds := ParseFile()
	for _, f := range folds {
		matrix = FoldMatrix(&matrix, f)
		counter := 0
		for i := 0; i < len(matrix); i++ {
			for j := 0; j < len(matrix[0]); j++ {
				if matrix[i][j] {
					counter++
				}
			}
		}
		fmt.Printf("There are %v dots\n", counter)
	}
	for i := 0; i < len(matrix); i++ {
		for j := 0; j < len(matrix[0]); j++ {
			if matrix[i][j] {
				print("# ")
			} else {
				print(". ")
			}
		}
		print("\n")
	}
}

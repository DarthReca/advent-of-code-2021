package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type Matrix [10][10]int
type Point [2]int

func ParseFile() Matrix {
	filebuffer, err := ioutil.ReadFile("input.txt")
	if err != nil {
		os.Exit(1)
	}
	data := bufio.NewScanner(strings.NewReader(string(filebuffer)))
	data.Split(bufio.ScanRunes)
	var matrix Matrix
	currentRow := 0

	for column := 0; data.Scan() && currentRow < 10; column++ {
		i, err := strconv.Atoi(data.Text())
		if err == nil {
			matrix[currentRow][column] = i
		} else if data.Text() == "\n" {
			currentRow++
			column = -1
		}
	}
	return matrix
}

func Adjacents(row int, column int) []Point {
	allPoints := []Point{
		{row - 1, column},
		{row + 1, column},
		{row, column - 1},
		{row, column + 1},
		{row + 1, column + 1},
		{row + 1, column - 1},
		{row - 1, column + 1},
		{row - 1, column - 1},
	}
	points := make([]Point, 0, 8)
	for _, p := range allPoints {
		if p[0] >= 0 && p[1] >= 0 && p[0] < 10 && p[1] < 10 {
			points = append(points, p)
		}
	}
	return points
}

func Step(matrix *Matrix) (int, bool) {
	// Update everything
	var flashMask [10][10]bool
	everyoneFlashed := true
	for r := 0; r < 10; r++ {
		for c := 0; c < 10; c++ {
			matrix[r][c]++
			flashMask[r][c] = false
		}
	}
	// Count the totalFlashes
	totalFlashes := 0
	for currentFlashes := 0; ; currentFlashes = 0 {
		for r := 0; r < 10; r++ {
			for c := 0; c < 10; c++ {
				if matrix[r][c] > 9 && !flashMask[r][c] {
					currentFlashes++
					for _, p := range Adjacents(r, c) {
						matrix[p[0]][p[1]]++
					}
					flashMask[r][c] = true
				}
			}
		}
		if currentFlashes == 0 {
			break
		}
		totalFlashes += currentFlashes
	}
	// Reset flashes
	for r := 0; r < 10; r++ {
		for c := 0; c < 10; c++ {
			if matrix[r][c] > 9 {
				flashMask[r][c] = true
				matrix[r][c] = 0
			}
			everyoneFlashed = everyoneFlashed && flashMask[r][c]
		}
	}
	return totalFlashes, everyoneFlashed
}

func main() {
	mat := ParseFile()
	totalFlashes := 0
	for i := 0; ; i++ {
		currentFlashes, everyoneFlashed := Step(&mat)
		if everyoneFlashed {
			fmt.Println("Flash simultaneusly at ", i+1)
			break
		}
		totalFlashes += currentFlashes
	}
	fmt.Println("Total: ", totalFlashes)
}

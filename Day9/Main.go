package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"os"
	"sort"
	"strconv"
	"strings"
)

type Point = [2]int

func ParseFile() [][]int {
	filebuffer, err := ioutil.ReadFile("input.txt")
	if err != nil {
		os.Exit(1)
	}
	data := bufio.NewScanner(strings.NewReader(string(filebuffer)))
	data.Split(bufio.ScanRunes)
	matrix := make([][]int, 1)
	currentRow := 0

	for data.Scan() {
		i, err := strconv.Atoi(data.Text())
		if err == nil {
			matrix[currentRow] = append(matrix[currentRow], i)
		} else if data.Text() == "\n" {
			currentRow++
			matrix = append(matrix, make([]int, 0))
		}
	}
	return matrix
}

// Search for the positions of the bigger values sorrounding a given point
func FindBiggerNeighbors(matrix [][]int, point Point) []Point {
	r, c := point[0], point[1]
	var selected []Point
	if r != 0 && matrix[r-1][c] > matrix[r][c] {
		selected = append(selected, [2]int{r - 1, c})
	}
	if r < len(matrix)-1 && matrix[r+1][c] > matrix[r][c] {
		selected = append(selected, [2]int{r + 1, c})
	}
	if c != 0 && matrix[r][c-1] > matrix[r][c] {
		selected = append(selected, [2]int{r, c - 1})
	}
	if c < len(matrix[r])-1 && matrix[r][c+1] > matrix[r][c] {
		selected = append(selected, [2]int{r, c + 1})
	}
	return selected
}

func FindLowPoints(matrix [][]int) []Point {
	var points []Point
	for r := range matrix {
		for c, v := range matrix[r] {
			isLowPoint := true
			if r != 0 {
				isLowPoint = isLowPoint && matrix[r-1][c] > v
			}
			if r < len(matrix)-1 {
				isLowPoint = isLowPoint && matrix[r+1][c] > v
			}
			if c != 0 {
				isLowPoint = isLowPoint && matrix[r][c-1] > v
			}
			if c < len(matrix[r])-1 {
				isLowPoint = isLowPoint && matrix[r][c+1] > v
			}
			// Add if this is low point
			if isLowPoint {
				points = append(points, [2]int{r, c})
			}
		}
	}
	return points
}

func FindBasins(matrix [][]int, lowPoints []Point) []map[Point]bool {
	var basins []map[Point]bool
	for _, lowPoint := range lowPoints {
		basins = append(basins, map[Point]bool{lowPoint: true})
		for bigNeighbor := FindBiggerNeighbors(matrix, lowPoint); len(bigNeighbor) != 0; {
			head := bigNeighbor[0]
			if matrix[head[0]][head[1]] != 9 {
				basins[len(basins)-1][head] = true
			}
			bigNeighbor = append(bigNeighbor[1:], FindBiggerNeighbors(matrix, head)...)
		}
	}
	return basins
}

func CalculateRisk(matrix [][]int, points []Point) int {
	sum := 0
	for _, p := range points {
		sum += 1 + matrix[p[0]][p[1]]
	}
	return sum
}

func main() {
	matrix := ParseFile()
	lowPoints := FindLowPoints(matrix)
	fmt.Println(CalculateRisk(matrix, lowPoints))
	// Part 2
	basins := FindBasins(matrix, lowPoints)
	largest := make([]int, 0, len(basins))
	for _, basin := range basins {
		largest = append(largest, len(basin))
	}
	sort.Sort(sort.Reverse(sort.IntSlice(largest)))
	result := 1
	for _, v := range largest[0:3] {
		result *= v
	}
	fmt.Println(result)
}

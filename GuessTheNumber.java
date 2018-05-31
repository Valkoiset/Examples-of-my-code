package com.company;

import java.util.Scanner;

public class GuessTheNumber {

    public static void main(String[] args) {

        GuessTheNumber main = new GuessTheNumber();
        main.task26(1,10);
    }

    public void task26(int a, int b) {     // a, b - are boarders of random number


        int random_number = a + (int) (Math.random() * b);
        Scanner myScanner = new Scanner(System.in);
        System.out.println("Input number >>> ");
        int user_number = myScanner.nextInt();

        while (user_number != random_number)
            if (user_number > random_number) {
                System.out.println("less");
                System.out.println("Input number >>> ");
                user_number = myScanner.nextInt();
            } else if (user_number < random_number) {
                System.out.println("more");
                System.out.println("Input number >>> ");
                user_number = myScanner.nextInt();
            }
        System.out.println(" Bingo!");
    }
}

/* Here is the simple game written by me where user has to guess the number from a to b.
   I used such Java operators as if/else and while */

package com.company;

import java.util.Scanner;

public class GuessTheNumber {

    public static void main(String[] args) {

        GuessTheNumber main = new GuessTheNumber();
        main.guessTheNumber(1,10);
    }

    public void guessTheNumber(int a, int b) {     // a, b - are borders of random number


        int random_number = a + (int) (Math.random() * b);      // Initializing random number
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

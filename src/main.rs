// These imports are for measureing time
use std::time::Instant;

// Parallel stuff
use rayon::prelude::*;
use rayon::ThreadPoolBuilder;
use indicatif::{ProgressBar, ProgressStyle, ParallelProgressIterator};


struct ExpressionTree {
    is_op: bool, // If its an operator, true, else false
    op_code: Option<u8>, // 0 : concat or nothing, 1: add, 2: sub, 3: mul, 4: div, 5: expo
    value: Option<*mut i32>, // If not operator, then this is the value
    l: Option<Box<ExpressionTree>>, // Lower expressions. If op_code true, this and r must exist
    r: Option<Box<ExpressionTree>>
}

const CONCAT: u8 = 5;
const ADD: u8 = 0;
const SUB: u8 = 1;
const MUL: u8 = 2;
const DIV: u8 = 3;
const EXPO: u8 = 4;

// DOes nothing
// const WASTER: u8 = 1;



// USE BASE 6 IF YOU WANT WITH CONCAT< BASE 5 OTHERWISE
const BASE: i32 = 6;


/**
    Returns a tuple containing if it failed, and the value
*/
fn evaluate_expression(exp_tree: &ExpressionTree) -> (bool, i32){
    if exp_tree.is_op {
        // Something something borrowing
        let left_value = evaluate_expression(exp_tree.l.as_ref().expect("THE EXPRESSION HAS NO LEFT REE!"));
        // Check this earlier, so don't have to waste performance evaluating the other side (short circuit yeaaa boi)
        if left_value.0 {
            return (true,0);
        }
        let right_value = evaluate_expression(exp_tree.r.as_ref().expect("THE EXPRESSION HAS NO RIGHT REE!"));

        // If either of them failed, no need to evaluate anything
        if right_value.0 {
            return (true, 0);
        }

        let op_code = exp_tree.op_code.expect("Somehow no operator wtf");
        let final_output = match op_code {
            // Concatenation
            CONCAT => {
                // Concatenate the numbers
                // Left multiplied by (10*(digits of right))+right
                let r_length = (right_value.1).checked_ilog10().unwrap_or(0) + 1;
                let output = (left_value.1)*(i32::pow(10i32,r_length))+right_value.1;
                (false, output)
            }
            // +
            ADD => (false, left_value.1+right_value.1),
            // -
            SUB => (false, left_value.1-right_value.1),
            // *
            MUL => (false, left_value.1 * right_value.1),
            // divide
            // Maybe should check divisibility first
            DIV => {
                if right_value.1 == 0 {(true, 0)} 
                else {
                    if left_value.1 % right_value.1 == 0 {(false, left_value.1 / right_value.1)}
                    else {(true, 0)}
                }
            },
            // exponentiation
            EXPO => {
                // The right side of or should never occur hopefully, but its there just in case
                if right_value.1 > 12 || right_value.1 < 0 {(false, 0)}
                else {(false, i32::pow(left_value.1, right_value.1 as u32))}
            }
            _ => (true,0)
        };

        return final_output;
    } else {
        // Numeric literals
        // OMG UNSAFE
        unsafe{
            return (false, *(exp_tree.value.expect("If this is not a number, gg")));
        }
    }
}

/**
 * The length is the number of operators it should have
 */
fn create_expression_tree(index: i32, sample_reference: &Vec<*mut i32>) -> ExpressionTree{
    // Make literally trees list

    let length = (*sample_reference).len()-1;
    
    // TODO: TRY STACK HERE? OR LINKED LIST! FASTER INSERT AND DELETION?
    // IT IS APPARENTLY BAD MAYBE?

    let mut tree_list : Vec<ExpressionTree> = Vec::new();
    for value in sample_reference {
        tree_list.push(ExpressionTree { is_op: false, op_code: None, value: Some(*value), l: None, r: None});
    }


    let mut op_set = number_to_base(index, length, BASE);


    // Then remove concat, make concat tree, put it back in trees list where it was
    // keep doing until single element and so on yay
    // Same for other operators
    // And also this works!!!!!!!! No hanging elements! LESSS GO

    // First concatenation
    while op_set.contains(&CONCAT) {
        // If this panics, i have no idea what to do
        let ind = op_set.iter().position(|&x| x == CONCAT).unwrap();
        op_set.remove(ind);
        // Ya ik slow, but oh well
        let right_tree = tree_list.remove(ind+1);
        let left_tree = tree_list.remove(ind);
        tree_list.insert(ind, ExpressionTree { is_op: true, op_code: Some(CONCAT), value: None, l: Some(Box::new(left_tree)), r: Some(Box::new(right_tree))});
    }

    // Exponentiation
    while op_set.contains(&EXPO) {
        // If this panics, i have no idea what to do
        let ind = op_set.iter().position(|&x| x == EXPO).unwrap();
        op_set.remove(ind);
        // Ya ik slow, but oh well
        let right_tree = tree_list.remove(ind+1);
        let left_tree = tree_list.remove(ind);
        tree_list.insert(ind, ExpressionTree { is_op: true, op_code: Some(EXPO), value: None, l: Some(Box::new(left_tree)), r: Some(Box::new(right_tree))});
    }

    // Div and multiplication, whichever comes first
    while op_set.contains(&MUL) || op_set.contains(&DIV) {
        let mut ind_mul: usize = 100000;
        if op_set.contains(&MUL){ind_mul = op_set.iter().position(|&x| x == MUL).unwrap();}
        let mut ind_div: usize = 100000;
        if op_set.contains(&DIV) {ind_div = op_set.iter().position(|&x| x == DIV).unwrap();}
        let (ind,operation) = if ind_mul<ind_div  {(ind_mul,MUL)} else {(ind_div,DIV)};
        op_set.remove(ind);

        let right_tree = tree_list.remove(ind+1);
        let left_tree = tree_list.remove(ind);
        tree_list.insert(ind, ExpressionTree { is_op: true, op_code: Some(operation), value: None, l: Some(Box::new(left_tree)), r: Some(Box::new(right_tree))});
    }

    // Add sub, whichever comes first
    while op_set.contains(&ADD) || op_set.contains(&SUB) {
        let mut ind_add: usize = 10000;
        let mut ind_sub: usize = 10000;
        if op_set.contains(&ADD) {ind_add = op_set.iter().position(|&x| x == ADD).unwrap();}
        if op_set.contains(&SUB) {ind_sub = op_set.iter().position(|&x| x == SUB).unwrap();}
        let (ind,operation) = if ind_add<ind_sub  {(ind_add,ADD)} else {(ind_sub,SUB)};
        op_set.remove(ind);

        let right_tree = tree_list.remove(ind+1);
        let left_tree = tree_list.remove(ind);
        tree_list.insert(ind, ExpressionTree { is_op: true, op_code: Some(operation), value: None, l: Some(Box::new(left_tree)), r: Some(Box::new(right_tree))});
    }


    // At end, only one index remains
    return tree_list.remove(0);
}

// 0 to 5 values
fn number_to_base(number: i32, expected_length: usize, base: i32) -> Vec<u8> {
    let mut num = number;
    let mut digits = Vec::new();
    if num == 0 {
        digits.push(0);
    } else {
        while num > 0 {
            digits.push((num % base) as u8);
            num /= base;
        }
        digits.reverse();
    }
    while digits.len() < (expected_length) {
        digits.insert(0, 0);
    }
    digits
}


// Given a list of pointers, this puts the digits at those pointer locations
fn insert_to_pointers(mut n: i32, digits: usize, references: &Vec<*mut i32>) {

    // Extract digits in reverse (least significant first)
    let mut temp = vec![0i32; digits];
    for i in (0..digits).rev() {
        temp[i] = n % 10;
        n /= 10;
    }

    // Write to the provided pointers
    unsafe {
        for (i, &ptr) in references.iter().enumerate() {
            *ptr = temp[i] as i32;
        }
    }
}

fn alternatestring(number: i32, ops: Vec<char>) -> String{
    // Assuming the first one is the digits, and the second is the operators
    let mut output_str = String::new();
    let num_str = number.to_string();
    let mut num_str_chars = num_str.chars();
    let lenop = ops.len();
    for index in 0..lenop {
        // nth 0, as it gets consumed
        output_str.push(num_str_chars.nth(0).unwrap());
        if *ops.get(index).unwrap() != ' ' {
            output_str.push(*ops.get(index).unwrap());
        }
    }
    // Last number
    output_str.push(num_str_chars.nth(0).unwrap());
    return output_str;
}


fn main() {
    let number_of_threads = 16;
    ThreadPoolBuilder::new()
        .num_threads(number_of_threads) // <-- Set your desired thread limit here
        .build_global()
        .expect("Failed to build global thread pool");


    println!("Hello, world!");
    let start = Instant::now();
    
    let digit_count = 6;

    
    // Number of indexes = (digitcount-1)^6 ?

    let max_index = BASE.pow((digit_count-1) as u32);
    println!("Number of iterations: {}", max_index);

    let min_num = 10i32.pow((digit_count-1) as u32);
    let max_num = 10i32.pow((digit_count) as u32);
    // For now, as 8 digits are too crazy
    //let max_num = 2*10i32.pow((digit_count-1) as u32);



    // Progress bar
    let pb = ProgressBar::new(max_index as u64);

    pb.set_style(ProgressStyle::default_bar()
        .template("{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} ({percent}%) {per_sec} ETA: {eta_precise}")
        .unwrap()
        .progress_chars("#>-"));



    // Main loop
    // DOing till -1 to exclude all concat
    let mut nice_vec: Vec<(i32,i32)> = (0..(max_index-1)).into_par_iter().progress_with(pb).flat_map(|tree_index| {
        


        let mut sample_numbers = vec![0;digit_count];
        let mut sample_reference: Vec<*mut i32> = Vec::new();

        // Get a mutable raw pointer to the start of the Vec's data
        let base_ptr = sample_numbers.as_mut_ptr();

        // Iterate through the Vec and calculate the pointer for each element
        for i in 0..sample_numbers.len() {
            unsafe {
                // Use pointer arithmetic to get the address of each element
                let ptr_to_element = base_ptr.add(i);
                sample_reference.push(ptr_to_element);
            }
        }

        let mut nice_local_vec: Vec<(i32,i32)> = Vec::new();


        let tree = create_expression_tree(tree_index, &sample_reference);

        for i in min_num..max_num{
            insert_to_pointers(i, digit_count, &sample_reference);
            let value = evaluate_expression(&tree);
            if !value.0 {
                if value.1 == (i as i32) {
                    //println!("{:?}", value);
                    nice_local_vec.push((value.1, tree_index));
                }
            }
        }
        nice_local_vec
    }).collect();
    let duration = start.elapsed();


    nice_vec.sort_by(|a, b| a.0.cmp(&b.0));
    println!("List of expressions found: ");
    for element in nice_vec {
        let op_list = number_to_base(element.1, digit_count-1, BASE);
        let mut ops_chars: Vec<char> = Vec::new();
        
        for op in op_list {
            let mut push_op = 'a';
            if op == EXPO {
                push_op = '^'
            } else if op == DIV {
                push_op = '/'
            } else if op == MUL {
                push_op = '*'
            } else if op == SUB {
                push_op = '-'
            } else if op == ADD {
                push_op = '+'
            } else if op == CONCAT {
                push_op = ' '
            }
            ops_chars.push(push_op);
        }

        let str_exp = alternatestring(element.0, ops_chars.clone());




        println!("{};{:?};{}=={}", element.0, ops_chars,element.0, str_exp)
    }

    println!("Time taken: {:?}", duration); 
    println!("Time taken in milliseconds: {}ms", duration.as_millis());


}

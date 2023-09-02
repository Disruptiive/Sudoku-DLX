global ∞ = typemax(Int32);
global COLUMN_N = 9*9*4

mutable struct Node
    U::Union{Node,Nothing}
    D::Union{Node,Nothing}
    R::Union{Node,Nothing}
    L::Union{Node,Nothing}
    C::Union{Node,Nothing}
    S::Union{Int32,Nothing}
    N::Union{String,Nothing}
end

function traverse9x9Matrix(row,col)
    if col == 9
        col = 1;
        row += 1;
    else
        col += 1;
    end
    return row,col
end

function strToVecs(str)
    rows = [];
    cols = [];
    nums = [];

    row_i = 1;
    col_i = 1;
    for c in str
        if c == '.'
            row_i,col_i = traverse9x9Matrix(row_i,col_i);
        else
            push!(rows,row_i);
            push!(cols,col_i);
            num = parse(Int8,c);
            push!(nums,num);
            row_i,col_i = traverse9x9Matrix(row_i,col_i);
        end
    end
    return rows,cols,nums;
end

function createNode(up_node=nothing,down_node=nothing,right_node=nothing,left_node=nothing, list_node = nothing, size = nothing, Name = nothing)
    return Node(up_node,down_node,right_node,left_node, list_node, size, Name);
end

function createHead()
    return createNode(nothing, nothing, nothing, nothing, nothing, nothing, "root");
end

function createColumn(id=nothing)
    new_col = createNode(nothing, nothing, nothing, nothing, nothing, 0, id);
    new_col.C = new_col;
    new_col.U = new_col;
    new_col.D = new_col;
    return new_col;
end

function createNode(up=nothing, left = nothing, list = nothing, name= nothing)
    return createNode(up, list, nothing, left, list, nothing, name);
end

function gotoCol(head,idx)
    if idx > COLUMN_N/2
        traverse_left = COLUMN_N - idx + 1;
        curr_col = head;
        for _i=1:traverse_left
            curr_col = curr_col.L;
        end
    else
        curr_col = head;
        for _i=1:idx
            curr_col = curr_col.R;
        end
    end
    return curr_col
end

function populateRow(row,col,num,root)
    amount_of_rows = amount_of_cols = amount_of_numbers = 9;
    block_size = 3;

    node_name = "R"*string(row)*"C"*string(col)*"#"*string(num);

    #Row-Column constraint
    rc_con = (row-1) * amount_of_cols + col;
    col_idx = gotoCol(root,rc_con);
    last_node = col_idx.U;
    new_node_rc = createNode(last_node, nothing, col_idx,node_name);
    last_node.D = new_node_rc;
    col_idx.U = new_node_rc;
    col_idx.S += 1;
                
    #Row-Number constraint
    rn_con = 81 + (row-1) * amount_of_numbers + num;
    col_idx = gotoCol(root,rn_con);
    last_node = col_idx.U;
    new_node_rn = createNode(last_node, new_node_rc, col_idx, node_name);
    new_node_rc.R = new_node_rn;
    last_node.D = new_node_rn;
    col_idx.U = new_node_rn;
    col_idx.S += 1;

    #Column-Number constraint
    cn_con = 81*2 + (col-1) * amount_of_numbers + num;
    col_idx = gotoCol(root,cn_con);
    last_node = col_idx.U;
    new_node_cn = createNode(last_node, new_node_rn, col_idx, node_name);
    new_node_rn.R = new_node_cn;
    last_node.D = new_node_cn;
    col_idx.U = new_node_cn;
    col_idx.S += 1;

    #Box-Number constraint
    block_x = (row-1) ÷ 3;
    block_y = (col-1) ÷ 3;
    block = block_x * block_size + block_y;
    bn_con = 81*3 + block * amount_of_numbers + num;
    col_idx = gotoCol(root,bn_con);
    last_node = col_idx.U;
    new_node_bn = createNode(last_node, new_node_cn, col_idx, node_name);
    new_node_cn.R = new_node_bn;
    last_node.D = new_node_bn;
    col_idx.U = new_node_bn;
    col_idx.S += 1;

    new_node_rc.L = new_node_bn;
    new_node_bn.R = new_node_rc;
end

function createGrid(row,col,num,root)
    i = 1
    for row_i = 1:9
        for col_i = 1:9
            if row_i == row[i] && col_i == col[i]
                populateRow(row[i],col[i],num[i],root);
                if length(row) > i 
                    i+=1;
                end
            else
                for n=1:9
                    populateRow(row_i,col_i,n,root);
                end
            end
        end
    end
end
function createCoverMatrix(row,col,num)
    # 9*9*9 rows, 
    amount_of_rows = amount_of_cols = amount_of_numbers = 9;
    block_size = 3;

    root = createHead();
    curr_node = root;
    for i = 1:COLUMN_N
        new_node = createColumn(string(i));
        new_node.L = curr_node;
        curr_node.R = new_node;
        curr_node = new_node;
    end
    root.L = curr_node;
    curr_node.R = root;
    
    createGrid(row,col,num,root);
    return root
end

function removeEmptyColumns(root)
    r = head.R;
    while(r!=head)
        if r.S == 0
            r.L.R = r.R;
            r.R.L = r.L;
        end
        r = r.R;
    end
end

function cover(col_to_cover)
    col_to_cover = col_to_cover.C;
    col_to_cover.R.L = col_to_cover.L;
    col_to_cover.L.R = col_to_cover.R;

    col_down = col_to_cover.D;
    while(col_down != col_to_cover)
        node_traverse = col_down.R;
        while(node_traverse != col_down)
            node_traverse.D.U = node_traverse.U;
            node_traverse.U.D = node_traverse.D;
            node_traverse.C.S -= 1;
            node_traverse = node_traverse.R;
        end
        col_down = col_down.D;
    end
    return col_to_cover.C
end

function uncover(col_to_uncover)
    col_to_uncover = col_to_uncover.C;
    col_up = col_to_uncover.U;
    while(col_up != col_to_uncover)
        node_traverse = col_up.L;
        while(node_traverse != col_up)
            node_traverse.C.S += 1;
            node_traverse.D.U = node_traverse;
            node_traverse.U.D = node_traverse;
            node_traverse = node_traverse.L;
        end
        col_up = col_up.U;
    end    
    col_to_uncover.C.R.L = col_to_uncover.C;
    col_to_uncover.C.L.R = col_to_uncover.C;
end

function heuristicSize(root)
    curr_col = root.R;
    picked_node = root;
    curr_size = ∞;
    while(curr_col != root)
        if (curr_col.S < curr_size)
            curr_size = curr_col.S;
            picked_node = curr_col;
        end
        curr_col = curr_col.R;
    end
    return picked_node;
end

function printSet(set)
    solutions = zeros(Int8,9,9);
    for i = 1:length(set)
        r = parse(Int32,(set[i].N)[2]);
        c = parse(Int32,(set[i].N)[4]);
        n = parse(Int32,(set[i].N)[6]);
        solutions[r,c] = n;
    end
    for row = 1:9
        for col = 1:3
            for block = 1:3
                actual_j = (col-1)*3 + block;
                print(solutions[row,actual_j]);
                print(" ");
            end
            if col != 3
                print("|");
            end
        end
        print("\n");
        if (row == 3 || row == 6)
            print(repeat("-",20),"\n");
        end
    end
end

function search(k,head,set)
    if (head.R == head)  
        printSet(set);
        return set;
    end
    node = heuristicSize(head);
    cover(node);
    down_node = node.D;
    while(down_node != node)
        push!(set,down_node);
        right_node = down_node.R;
        while(right_node != down_node)
            cover(right_node.C);
            right_node = right_node.R;
        end
        search(k+1,head,set);
        up_node = pop!(set);
        left_node = up_node.L;
        while(left_node != up_node)
            uncover(left_node.C);
            left_node = left_node.L;
        end
        down_node = down_node.D;
    end
    uncover(node);
end

sol_set = []
rows,cols,nums = strToVecs("..5..4..7..4.27.6..1.3.9.....2...8499.......64...............5..2.17....3..94...1");
head = createCoverMatrix(rows,cols,nums);
removeEmptyColumns(head);
search(1,head,sol_set)


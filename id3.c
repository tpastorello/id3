#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

struct node
{
    int data;
    char name[15];
    struct node *next;
};

struct deci_tree
{
    int data;
    int datasetCol;
    double ent;
    int pos;
    int childno;
    int ivalue;
    struct deci_tree *c[50];
} root;

struct node *createnewnode(struct node *head, char name[15], int d)
{
    struct node *ptr = head, *temp;

    temp = (struct node *)malloc(sizeof(struct node));
    strcpy(temp->name, name);
    temp->data = d;
    temp->next = NULL;
    if (head == NULL)
        head = temp;

    else
    {
        while (ptr->next != NULL)
        {
            ptr = ptr->next;
        }
        ptr->next = temp;
    }

    return head;
}

int search(struct node *head, char name[15])
{
    struct node *temp = head;
    while (temp != NULL)
    {
        if (strcmp(temp->name, name) == 0)
            return temp->data;
        else
            temp = temp->next;
    }

    if (temp == NULL)
        return 3;
    else
        return 0;
}

void display(struct node *head) // function to display the mapping of name and integer
{
    struct node *temp = head;

    if (temp == NULL)
        return;
    while (temp != NULL)
    {
        printf("%d->%s\n", temp->data, temp->name);
        temp = temp->next;
    }
}

double entropy(int a[500][500], int obj, int attr, int attrpos, int val, struct deci_tree *head1)
{
    int x, k;
    x = a[1][attr - 1];
    struct deci_tree *temp;
    temp = head1;
    int i;
    double b, c, d, total = 1.0;
    double count1 = 0, count2 = 0;

    if (temp == NULL && attrpos == attr - 1)
    {
        for (i = 1; i < obj; i++)
        {
            if (a[i][attrpos] == x)
                count1++;
            else
                count2++;
        }
    }

    if (temp == NULL && attrpos != attr - 1)
    {

        for (i = 1; i < obj; i++)
        {
            if (a[i][attrpos] == val)
            {
                if (a[i][attr - 1] == x)
                    count1 = count1 + 1;

                else
                    count2 = count2 + 1;
            }
        }
    }
    b = count1 / (count1 + count2);
    c = count2 / (count1 + count2);

    if (count1 == 0.000000 || count2 == 0.000000)
        d = 0;
    else
        d = ((count1 + count2) / (obj - 1)) * (-(b * (log(b) / log(2)) + c * (log(c) / log(2))));

    return d;
}

void findk(int a[500][500], int b[50][50], int obj, int attr)
{

    int i, j, k, count = 1;
    int flag = 0;

    for (k = 0; k < attr; k++)
    {
        b[k][1] = a[1][k];
        i = 1;
        while (i < obj)
        {
            for (j = 1; j < count; j++)
            {
                if (a[i][k] == b[k][j])
                {
                    flag = 1;
                }
            }
            if (flag == 0)
            {
                b[k][count] = a[i][k];
                count++;
            }
            flag = 0;
            i++;
        }
        b[k][0] = count - 1;

        count = 1;
    }
}

int findmax(double *gain1, int attr)
{

    int i = 0;
    int max1 = 0;
    double max;
    max = gain1[0];
    for (i = 1; i <= attr; i++)
    {
        if (gain1[i] > max)
        {
            max1 = i;
            max = gain1[i];
        }
    }
    return max1;
}

double findsum(double col1[50], int len)
{

    int i, j;
    double sum = 0;
    for (i = 0; i < len; i++)
    {
        sum = sum + col1[i];
    }
    return sum;
}

int *funcModiA(int a[500][500], int modifiedA[500][500], int max1, int attr, int obj, int val, int dim[2])
{

    int i, j, k;
    int row = 0, col = 0, temp = 0, col2 = 0;

    for (j = 0; j < obj; j++)
    {
        if (j == 0)
        {
            for (k = 0; k < attr;)
            {
                if (k == max1)
                {
                    k++;
                }
                else
                {
                    modifiedA[row][col2] = a[j][k];
                    k++;
                    col2++;
                }
            }
            row++;
        }
        else
        {
            if (a[j][max1] == val)
            {
                for (i = 0; i < attr;)
                {
                    if (i == max1)
                    {
                        i++;
                    }
                    else
                    {
                        modifiedA[row][col] = a[j][i];
                        temp = col++;
                        i++;
                    }
                }
                col = 0;
                row++;
            }
        }
    }
    dim[0] = row;
    dim[1] = attr - 1;
    return dim;
}

struct deci_tree *infoGainRecursive(int Z[500][500], int b[50][50], struct deci_tree *parent, double entr, int max1, int attr, int obj, int flag, int initattr, int a[500][500])
{

    int i = 0;
    int j;
    int modifiedA[500][500], modifiedB[50][50];
    double col3[50], col1[50], entr1[50], entropy1[50][50], gain1[50];
    int dim[2];
    if (flag == 1)
    {
        flag = 0;
        parent->ent = entr;
        parent->data = 0;
        for (i = 0; i < attr; i++)
        {
            for (j = 0; j < obj; j++)
            {
                modifiedA[j][i] = Z[j][i];
            }
        }
        findk(modifiedA, modifiedB, obj, attr);
    }

    else
    {

        for (i = 0; i < attr; i++)
        {

            for (j = 0; j < obj; j++)
            {
                modifiedA[j][i] = Z[j][i];
            }
        }

        funcModiA(modifiedA, modifiedA, max1, attr, obj, parent->pos, dim);
        obj = dim[0];
        attr = dim[1];
        parent->ent = entropy(modifiedA, obj, attr, i, modifiedB[i][j], NULL);
        if (parent->ent == 0)
        {
            int sd = parent->data;
            parent->data = modifiedA[1][attr - 1];
            parent->datasetCol = attr - 1;
            parent->childno = 0;
            printf("\n\tnode %d is terminated with row value %d ,  child of %d i posotion value %d\n", parent->data, parent->pos, sd, parent->ivalue);
            return 0;
        }
        else
        {
            findk(modifiedA, modifiedB, obj, attr);
        }
    }

    for (i = 0; i < attr - 1; i++)
    {
        for (j = 1; j <= modifiedB[i][0]; j++)
        {
            col3[j - 1] = entropy(modifiedA, obj, attr, i, modifiedB[i][j], NULL);
        }
        entr1[i] = findsum(col3, modifiedB[i][0]);
        gain1[i] = parent->ent - entr1[i];
    }

    int temp = max1;
    max1 = findmax(gain1, attr - 1);
    parent->data = modifiedA[0][max1];
    parent->childno = modifiedB[max1][0];

    for (i = 0; i <= initattr; i++)
    {
        if (parent->data == a[0][i])
        {
            parent->datasetCol = i;
            break;
        }
    }

    printf("\n\tCurrent node is %d and is child of %d , through  row value %d,i position %d coloum number %d \n", parent->data, Z[0][temp], parent->pos, parent->ivalue, parent->datasetCol);

    for (i = 1; i <= modifiedB[max1][0]; i++)
    {
        parent->c[i] = (struct deci_tree *)malloc(sizeof(struct deci_tree));
        parent->c[i]->pos = modifiedB[max1][i];
        parent->c[i]->data = modifiedA[0][max1];
        parent->c[i]->ent = parent->ent;
        parent->c[i]->ivalue = i;
        infoGainRecursive(modifiedA, modifiedB, parent->c[i], parent->c[i]->ent, max1, attr, obj, flag, initattr, a);
    }

    return parent;
}

int *get_test_value(int a[500][500], int attr, int obj)
{

    int i, j, flag = 0, *class, temp, counter = 1;
    temp = a[1][attr - 1];
    class = (int *)malloc(obj * sizeof(int));

    class[1] = temp;

    for (i = 2; i <= obj; i++)
    {
        for (j = 1; j <= counter; j++)
        {
            if (a[i][attr - 1] == class[j])
            {
                flag = 1;
                break;
            }
        }
        if (flag == 0)
        {
            counter++;
            class[counter] = a[i][attr - 1];
        }
        flag = 0;
    }

    class[0] = counter;
    printf("\n");
    return class;
}

int classification(int a[500][500], struct deci_tree *head, int test_data_pos, int *test_value)
{

    int temp, store, flag = 0, class, i;

    struct deci_tree *ptr;
    ptr = head;

    while (flag != 1)
    {
        temp = a[test_data_pos][ptr->datasetCol];
        for (i = 1; i <= ptr->childno; i++)
        {
            if (ptr->c[i]->pos == temp)
            {
                ptr = ptr->c[i];
            }
        }
        for (i = 1; i <= test_value[0]; i++)
        {
            if (test_value[i] == ptr->data)
            {
                class = test_value[i];
                flag = 1;
            }
        }
    }
    return class;
}

int main()
{

    struct node *first = NULL;
    struct deci_tree *head = NULL, *parent = NULL, *temphead = NULL;

    int i, j, k, t;
    int a[500][500], obj, attr, x, *d, training, test, *test_value, class;

    double r, database, error = 0, accuracy = 0, total;

    FILE *fp;

    char *tok;
    const char s[2] = ",";

    char buff[200];

    fp = fopen("golf.data", "r");

    k = 4;
    i = 0;

    while (fgets(buff, 200, fp) != NULL)
    {
        j = 0;
        tok = strtok(buff, s);
        while (tok != NULL)
        {
            t = search(first, tok);
            if (t != 3)
            {
                a[i][j] = t;
                j++;
            }
            if (t == 3) // this value is understood if it is checked in search method
            {
                first = createnewnode(first, tok, k);
                a[i][j] = k;
                j++;
                k++;
            }
            tok = strtok(NULL, s);
        }

        i++;
        attr = j;
    }
    obj = i;
    display(first);

    fclose(fp);

    d = (int *)malloc(attr * sizeof(int));
    for (k = 0; k < attr; k++)
    {
        d[k] = k;
    }
    for (i = 0; i < obj; i++)
    {
        printf("\n");
        for (j = 0; j < attr; j++)
        {
            printf("%d\t", a[i][j]);
        }
    }

    training = obj * 75 / 100;
    test = training;
    printf("\nTraining Data\n");
    for (i = 0; i < training; i++)
    {
        printf("\n");
        for (j = 0; j < attr; j++)
        {
            printf("%d\t", a[i][j]);
        }
    }
    printf("\nTest Data");
    for (i = test; i < obj; i++)
    {
        printf("\n");
        for (j = 0; j < attr; j++)
        {
            printf("%d\t", a[i][j]);
        }
    }

    database = entropy(a, training, attr, attr - 1, 0, NULL);
    int flag = 1;

    parent = (struct deci_tree *)malloc(sizeof(struct deci_tree));

    head = infoGainRecursive(a, NULL, parent, database, attr, attr, training, flag, attr, a);
    return 0;
}

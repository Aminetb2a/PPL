#!/usr/bin/python2


class DiscountCalc:
    def get_item_price(self):
        return input('Input a starting price (0 to quit): ')

    def discount(self, current_price, days):
        #pass # YOUR CODE HERE
        for i in range(days):
            x = (10*int(current_price))/100
            current_price = int(current_price) - x
            yield current_price



    def run(self):
        initial = self.get_item_price()
        for price in self.discount(initial, 10):
            print ("Price is discounted to : " + str(price))


DiscountCalc().run()

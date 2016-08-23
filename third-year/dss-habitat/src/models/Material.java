/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package models;

/**
 *
 * @author tiago
 */
public class Material extends BasicModel {
    private String name;
    private Integer quantity;

    public Material() {}

    public Material(String name, Integer quantity) {
        super(-1);
        this.name = name;
        this.quantity = quantity;
    }
    
    public Material(Material m){
        super(m.getId());
        this.name = m.getName();
        this.quantity = m.getQuantity();
    }

    public String getName() {
        return name;
    }

    public Integer getQuantity() {
        return quantity;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setQuantity(Integer quantity) {
        this.quantity = quantity;
    }

    @Override
    public Material clone(){
        return new Material(this);
    }

    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder(super.toString());
        sb.append(", ");
        sb.append(name);
        sb.append(", ");
        sb.append(quantity);
        
        return sb.toString();
    }
    
    @Override
    public boolean equals (Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass() ) return false;
       
        Material m = (Material) o;
    
        return ( super.equals(o) && this.name.equals(m.getName()) && this.quantity == m.getQuantity() );
    }
}
